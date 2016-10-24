{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Stackage.Package.Metadata
  ( module Stackage.Package.Metadata.Types
  , sinkPackageVersions
  , updateMetadata
  ) where

import qualified Codec.Archive.Tar as Tar
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Crypto.Hash (hashlazy, SHA256(..))
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, toLower, unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version (Version(Version))
import qualified Data.Yaml as Y (decodeEither, encode)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package
       (Dependency(..), PackageIdentifier(..), PackageName)
import Distribution.PackageDescription
       (CondTree(..), Condition(..), ConfVar(..),
        Flag(flagName, flagDefault), GenericPackageDescription, author,
        condBenchmarks, condExecutables, condLibrary, condTestSuites,
        description, genPackageFlags, homepage, license, maintainer,
        package, packageDescription, synopsis)
import Distribution.System (Arch(X86_64), OS(Linux))
import Distribution.Version
       (VersionRange, intersectVersionRanges, simplifyVersionRange,
        withinRange)
import Network.HTTP.Types (status200)
import Network.HTTP.Simple
import Prelude hiding (pi)
import System.FilePath (splitExtension, takeFileName, (<.>), (</>))

import Stackage.Package.Hashes
import Stackage.Package.Metadata.Types
import Stackage.Package.IndexConduit
import Stackage.Package.Git
import Stackage.Package.Locations

-- | Collects all available package versions along with preferred versions if
-- such are provided.
sinkPackageVersions
  :: Monad m
  => Consumer IndexEntry m (Map PackageName (Set Version, Maybe VersionRange))
sinkPackageVersions = CL.fold trackVersions Map.empty
  where
    updateVersionSetMap (newSet, _) (oldSet, moldRange) =
      (Set.union newSet oldSet, moldRange)
    -- in case that new range is Nothing, previous range is cleared.
    updateVersionRangeMap (_, mnewRange) (oldSet, _) = (oldSet, mnewRange)
    trackVersions versionsMap (VersionsEntry IndexFile {..}) =
      Map.insertWith
        updateVersionRangeMap
        ifPackageName
        (Set.empty, Just $ versionsPreferred ifFile)
        versionsMap
    trackVersions versionsMap (CabalEntry IndexFile {..}) =
      Map.insertWith
        updateVersionSetMap
        ifPackageName
        (Set.singleton (cabalVersion ifFile), Nothing)
        versionsMap
    trackVersions versionsMap _ = versionsMap

-- | Updates the metadata for packages using packages version information
-- produced by `sinkPackageVersions` and cabal files from the second repo.
updateMetadata
  :: (MonadIO m)
  => GitRepository -- ^ Matadata repository
  -> GitRepository -- ^ Repository with cabal files
  -> Map PackageName (Set Version, Maybe VersionRange) -- ^ Packages version
     -- information
  -> m ()
updateMetadata metadataRepo cabalFilesRepo pkgVersions =
  liftIO $
  do let fromVersions versionsMap
           | Map.null versionsMap = Nothing
           | otherwise = Just $ Map.deleteFindMin versionsMap
     let readCabalFile (pkgName, (versionSet, mversionRange)) = do
           let preferredVersionSet =
                 case mversionRange of
                   Nothing -> versionSet
                   Just range -> Set.filter (`withinRange` range) versionSet
           -- "From Hackage: If all the available versions of a package are
           -- non-preferred or deprecated, cabal-install will treat this the same
           -- as if none of them are."
           let preferredVersionSetNonEmpty =
                 if Set.null preferredVersionSet
                   then versionSet
                   else preferredVersionSet
           let pkgVersion = Set.findMax preferredVersionSetNonEmpty
           when (Set.null preferredVersionSet) $
             putStrLn $
             "Info: Package preferred version set is empty: " ++
             renderDistText pkgName ++
             ". Using all available versions for metadata."
           bls <-
             repoReadFile' cabalFilesRepo (getCabalFilePath pkgName pkgVersion)
           return
             (parseCabalFile pkgName pkgVersion bls, preferredVersionSetNonEmpty)
     CL.unfold fromVersions pkgVersions =$= CL.mapM readCabalFile $$
       CL.mapM_ (updatePackageIfChanged metadataRepo)

updatePackageIfChanged
  :: MonadIO m
  => GitRepository -> (CabalFile, Set Version) -> m ()
updatePackageIfChanged metadataRepo (CabalFile {..}, versionSet) =
  liftIO $
  do when (pkgVersionMax /= cfPackageVersion) $
       error $
       "Internal error, metadata package update version mismatch: " ++
       show (cfPackageName, cfPackageVersion, pkgVersionMax)
     when (package pd /= PackageIdentifier cfPackageName cfPackageVersion) $
       error $
       show ("mismatch" :: String, cfPackageName, cfPackageVersion, package pd)
     mepi <- fmap (Y.decodeEither . L.toStrict) <$> repoReadFile metadataRepo fp
     case mepi of
       Just (Right pi)
       -- Cabal file is the same and version preference list hasn't changed.
         | cabalHash == piHash pi && versionSet == piAllVersions pi -> return ()
       Just (Right pi)
       -- Current version hasn't changed, hence data in the sdist.tar.gz is stil
       -- the same, updating cabal related info only.
         | pkgVersionMax == piLatest pi ->
           repoWriteFile
             metadataRepo
             fp
             (L.fromStrict . Y.encode $
              pi
              { piLatest = pkgVersionMax
              , piHash = cabalHash
              , piAllVersions = versionSet
              , piSynopsis = pack $ synopsis pd
              , piAuthor = pack $ author pd
              , piMaintainer = pack $ maintainer pd
              , piHomepage = pack $ homepage pd
              , piLicenseName = pack $ renderDistText $ license pd
              })
       -- Version update or a totally new package.
       _ -> updatePackage
  where
    pkgNameStr = renderDistText cfPackageName
    pkgVersionStr = renderDistText pkgVersionMax
    pkgVersionMax = Set.findMax versionSet
    gpd = cfPackageDescription
    pd = packageDescription gpd
    url =
      concat
        [ mirrorFPComplete
        , "/package/"
        , pkgNameStr
        , "-"
        , pkgVersionStr
        , ".tar.gz"
        ]
    sink res
      | getResponseStatus res /= status200 = return Nothing
      | otherwise =
        fmap Just $ (CL.fold goEntry (pack $ description pd, "haddock", "", ""))
    updatePackage = do
      sdistReq <- parseRequest url
      result <- httpTarballSink sdistReq True sink
      case result of
        Nothing -> putStrLn $ "Skipping: " ++ url
        Just (desc, desct, cl, clt) -> do
          putStrLn $
            "Updating Metadata for package: " ++
            pkgNameStr ++ " to version: " ++ pkgVersionStr
          let checkCond = getCheckCond gpd
              getDeps' = getDeps checkCond
          repoWriteFile
            metadataRepo
            fp
            (L.fromStrict . Y.encode $
             PackageInfo
             { piLatest = pkgVersionMax
             , piHash = cabalHash
             , piAllVersions = versionSet
             , piSynopsis = pack $ synopsis pd
             , piDescription = desc
             , piDescriptionType = desct
             , piChangeLog = cl
             , piChangeLogType = clt
             , piBasicDeps =
               combineDeps $
               maybe id ((:) . getDeps') (condLibrary gpd) $
               map (getDeps' . snd) (condExecutables gpd)
             , piTestBenchDeps =
               combineDeps $
               map (getDeps' . snd) (condTestSuites gpd) ++
               map (getDeps' . snd) (condBenchmarks gpd)
             , piAuthor = pack $ author pd
             , piMaintainer = pack $ maintainer pd
             , piHomepage = pack $ homepage pd
             , piLicenseName = pack $ renderDistText $ license pd
             })
    fp =
      "packages" </> (unpack $ toLower $ pack $ take 2 $ pkgNameStr ++ "XX") </>
      pkgNameStr <.>
      "yaml"
    cabalHash = unDigest SHA256 $ hashlazy cfRaw
    goEntry :: (Text, Text, Text, Text) -> Tar.Entry -> (Text, Text, Text, Text)
    goEntry orig@(desc, desct, cl, clt) e =
      case (toEntryType $ Tar.entryPath e, toText $ Tar.entryContent e) of
        (ChangeLog clt', Just cl') -> (desc, desct, cl', clt')
        (Desc desct', Just desc') -> (desc', desct', cl, clt)
        _ -> orig
    toText (Tar.NormalFile lbs' _) =
      Just $ decodeUtf8With lenientDecode $ L.toStrict lbs'
    toText _ = Nothing

data EntryType
  = Ignored
  | ChangeLog Text
  | Desc Text
  deriving (Show)

-- | Only take things in the root directory of the tarball
toEntryType :: FilePath -> EntryType
toEntryType fp
  | length (filter (== '/') fp) /= 1 = Ignored
  | name == "changelog" = ChangeLog t
  | name == "changes" = ChangeLog t
  | name == "readme" = Desc t
  | otherwise = Ignored
  where
    (name', ext) = splitExtension fp
    name = unpack $ toLower $ pack $ takeFileName name'
    t =
      case ext of
        ".md" -> "markdown"
        ".markdown" -> "markdown"
        _ -> "text"

-- | FIXME these functions should get cleaned up and merged into stackage-common
getCheckCond :: GenericPackageDescription -> Condition ConfVar -> Bool
getCheckCond gpd = go
  where
    go (Var (OS os)) = os == Linux -- arbitrary
    go (Var (Arch arch)) = arch == X86_64 -- arbitrary
    go (Var (Flag flag)) = fromMaybe False $ Map.lookup flag flags -- arbitrary
    go (Var (Impl flavor range)) = flavor == GHC && ghcVersion `withinRange` range
    go (Lit b) = b
    go (CNot c) = not $ go c
    go (CAnd x y) = go x && go y
    go (COr x y) = go x || go y
    ghcVersion = Version [7, 10, 1] [] -- arbitrary
    flags = Map.fromList $ map toPair $ genPackageFlags gpd
      where
        toPair f = (flagName f, flagDefault f)

getDeps
  :: (Condition ConfVar -> Bool)
  -> CondTree ConfVar [Dependency] a
  -> Map PackageName VersionRange
getDeps checkCond = goTree
  where
    goTree (CondNode _data deps comps) =
      combineDeps $
      map (\(Dependency name range) -> Map.singleton name range) deps ++
      map goComp comps
    goComp (cond, yes, no)
      | checkCond cond = goTree yes
      | otherwise = maybe Map.empty goTree no

combineDeps :: [Map PackageName VersionRange] -> Map PackageName VersionRange
combineDeps =
  Map.unionsWith
    (\x y -> normalize . simplifyVersionRange $ intersectVersionRanges x y)
  where
    normalize vr =
      case parseDistText $ renderDistText vr of
        Nothing -> vr
        Just vr' -> vr'
