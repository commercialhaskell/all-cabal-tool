{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stackage.Package.Metadata
  ( module Stackage.Package.Metadata.Types
  , sinkPackageVersions
  , updateMetadata
  ) where

import qualified Codec.Archive.Tar as Tar
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, toLower, unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version (Version)
import qualified Data.Yaml as Y (decodeEither, encode)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Version (VersionRange, withinRange)
import Network.HTTP.Simple
import Network.HTTP.Types (status200)
import Prelude hiding (pi)
import Stackage.Package.Git
import Stackage.Package.IndexConduit
import Stackage.Package.Locations
import Stackage.Package.Metadata.Types
import System.FilePath (splitExtension, takeFileName, (<.>), (</>))
import System.IO (hPutStrLn, stderr)

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
  => Repositories -- ^ Repositories
  -> Map PackageName (Set Version) -- ^ Valid package version set.
  -> Map PackageName (Set Version, Maybe VersionRange) -- ^ Packages version
     -- information
  -> m ()
updateMetadata Repositories {..} validPackages packageVersions =
  liftIO $ do
    let fromVersions versionsMap
          | Map.null versionsMap = Nothing
          | otherwise = Just $ Map.deleteFindMin versionsMap
    let readCabalFile (packageName, (versionSet, mversionRange)) = do
          let preferredVersionSet =
                case mversionRange of
                  Nothing -> versionSet
                  Just range -> Set.filter (`withinRange` range) versionSet
           -- "From Hackage: If all the available versions of a package are
           -- non-preferred or deprecated, cabal-install will treat this the same
           -- as if none of them are."
          let preferredVersionSetValid =
                Set.intersection
                  (maybe Set.empty id $ Map.lookup packageName validPackages) $
                if Set.null preferredVersionSet
                  then versionSet
                  else preferredVersionSet
          if Set.null preferredVersionSetValid
            then do
              hPutStrLn stderr $
                "No valid versions found for: " ++ show (unPackageName packageName)
              return Nothing
            else do
              let packageVersionMax = Set.findMax preferredVersionSetValid
              let cabalFileName = getCabalFilePath packageName packageVersionMax
              cabalFile <-
                parseCabalFile cabalFileName <$> repoReadFile' allCabalFiles cabalFileName
              return $ Just (cabalFile, packageName, preferredVersionSetValid)
    CL.unfold fromVersions packageVersions =$= CL.mapMaybeM readCabalFile $$
      CL.mapM_ (updatePackageIfChanged allCabalMetadata)

updatePackageIfChanged
  :: MonadIO m
  => GitRepository -> (CabalFile, PackageName, Set Version) -> m ()
updatePackageIfChanged metadataRepo (cabalFile@CabalFile {..}, packageName, versionSet) =
  liftIO $
  do mepi <- fmap (Y.decodeEither . L.toStrict) <$> repoReadFile metadataRepo fp
     case mepi of
       Just (Right pi)
       -- Cabal file is the same and version preference list hasn't changed.
         | cfHash == piHash pi && versionSet == piAllVersions pi -> return ()
       Just (Right pi)
       -- Current version hasn't changed, hence data in the sdist.tar.gz is still
       -- the same, updating cabal related info only.
         | pkgVersionMax == piLatest pi -> do
           checkCabalFile
           repoWriteFile
             metadataRepo
             fp
             (L.fromStrict . Y.encode $
              makePackageInfo
                cabalFile
                versionSet
                (piDescription pi)
                (piDescriptionType pi)
                (piChangeLog pi)
                (piChangeLogType pi))
       -- Version update or a totally new package.
       _ -> updatePackage
  where
    pkgNameStr = renderDistText packageName
    pkgVersionStr = renderDistText pkgVersionMax
    pkgVersionMax = Set.findMax versionSet
    checkCabalFile = do
      let cabalPackageId = PackageIdentifier packageName pkgVersionMax
      let cabalFileName = getCabalFilePath packageName pkgVersionMax
      when (cabalPackageId /= cfPackage) $
        error $
        "Stackage.Package.Metadata.updateMetadata: Parsed cabal file: " ++
        cabalFileName ++
        " package identifier mismatch: " ++
        show cabalPackageId ++ " /= " ++ (show cfPackage)
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
        fmap Just $ (CL.fold goEntry (cfDescription, "haddock", "", ""))
    updatePackage = do
      checkCabalFile
      sdistReq <- parseRequest url
      result <- httpTarballSink sdistReq True sink
      case result of
        Nothing -> putStrLn $ "Skipping: " ++ url
        Just (desc, desct, cl, clt) -> do
          putStrLn $
            "Updating Metadata for package: " ++
            pkgNameStr ++ " to version: " ++ pkgVersionStr
          repoWriteFile
            metadataRepo
            fp
            (L.fromStrict . Y.encode $
             makePackageInfo cabalFile versionSet desc desct cl clt)
    fp =
      "packages" </> (unpack $ toLower $ pack $ take 2 $ pkgNameStr ++ "XX") </>
      pkgNameStr <.>
      "yaml"
    goEntry :: (Text, Text, Text, Text) -> Tar.Entry -> (Text, Text, Text, Text)
    goEntry orig@(desc, desct, cl, clt) e =
      case (toEntryType $ Tar.entryPath e, toText $ Tar.entryContent e) of
        (ChangeLog clt', Just cl') -> (desc, desct, cl', clt')
        (Desc desct', Just desc')  -> (desc', desct', cl, clt)
        _                          -> orig
    toText (Tar.NormalFile lbs' _) =
      Just $ decodeUtf8With lenientDecode $ L.toStrict lbs'
    toText _ = Nothing


makePackageInfo :: CabalFile -> Set Version -> Text -> Text -> Text -> Text -> PackageInfo
makePackageInfo CabalFile{..} versionSet desc descType cl clType =
  PackageInfo
  { piLatest = pkgVersion cfPackage
  , piHash = cfHash
  , piAllVersions = versionSet
  , piSynopsis = cfSynopsis
  , piDescription = desc
  , piDescriptionType = descType
  , piChangeLog = cl
  , piChangeLogType = clType
  , piBasicDeps = cfBasicDeps
  , piTestBenchDeps = cfTestBenchDeps
  , piAuthor = cfAuthor
  , piMaintainer = cfMaintainer
  , piHomepage = cfHomepage
  , piLicenseName = cfLicenseName
  }

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
        ".md"       -> "markdown"
        ".markdown" -> "markdown"
        _           -> "text"
