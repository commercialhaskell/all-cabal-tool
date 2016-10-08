{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Stackage.Package.Metadata.Update where

import qualified Codec.Archive.Tar as Tar
import Control.Exception (assert)
import Control.Exception.Enclosed (tryAny)
import Control.Monad (when, void, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (MonadResource, runResourceT, throwM)
import Crypto.Hash (hashlazy, SHA256(..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Conduit
import qualified Data.Conduit.List as CL
import Data.Aeson as A (encode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, toLower, unpack)
import Data.Text.Encoding (decodeUtf8With)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Version (Version(Version))
import Data.Yaml (decodeEither', decodeFileEither, encodeFile)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package
       (Dependency(..), PackageIdentifier(..), PackageName)
import Distribution.PackageDescription
       (CondTree(..), Condition(..), ConfVar(..),
        Flag(flagName, flagDefault), GenericPackageDescription, author,
        condBenchmarks, condExecutables, condLibrary, condTestSuites,
        description, genPackageFlags, homepage, license, maintainer,
        package, packageDescription, synopsis)
import Distribution.PackageDescription.Parse (ParseResult(..))
import Distribution.System (Arch(X86_64), OS(Linux))
import Distribution.Version
       (VersionRange, intersectVersionRanges, simplifyVersionRange,
        withinRange)
import Network.HTTP.Types (status200)
import Network.HTTP.Client
       (Manager, brConsume, newManager, responseBody, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Prelude hiding (pi)
import System.Directory
       (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath
       (splitExtension, takeDirectory, takeFileName, takeExtension, (<.>), (</>))
import System.IO (hClose)
import System.IO.Temp
import Data.Conduit.Zlib (ungzip)

import Stackage.Package.Hashes
import Stackage.Package.Metadata
import Stackage.Package.IndexConduit
import Stackage.Package.Locations


sinkPackageVersions
  :: Monad m
  => PreferredInfo
  -> Consumer CabalFileEntry m (Map PackageName (Set Version))
sinkPackageVersions preferredInfo = CL.fold keepPreferred Map.empty
  where
    keepPreferred newestMap cfe =
      let pkgName = cfeName cfe
          pkgVersion = cfeVersion cfe
      in if checkPreferred preferredInfo pkgName pkgVersion
           then flip (Map.insert pkgName) newestMap $
                case Map.lookup pkgName newestMap of
                  Nothing -> Set.singleton pkgVersion
                  Just versionsSet -> Set.insert pkgVersion versionsSet
                -- Skip the deprecated package version
           else newestMap


updateMetadata
  :: (MonadBaseControl IO m, MonadIO m) =>
     Repositories -> Map PackageName (Set Version) -> m ()
updateMetadata repos pkgVersions =
  runResourceT $
  CL.unfold fromVersions pkgVersions =$= CL.mapM readCabalFile $$
  CL.mapM_ (updatePackage $ allCabalMetadata repos)
  where
    fromVersions versionsMap
      | Map.null versionsMap = Nothing
      | otherwise = Just $ Map.deleteFindMin versionsMap
    readCabalFile (pkgName, versionSet) = do
      let pkgVersion = Set.findMax versionSet
      liftIO $
        putStrLn $
        "<Metadata> Versions for package: " ++
        renderDistText pkgName ++
        " are: " ++
        show (Set.map renderDistText versionSet) ++
        " and chose max: " ++ renderDistText pkgVersion
      bls <-
        liftIO $
        repoFileReader
          (allCabalHashes repos)
          (getCabalFilePath pkgName pkgVersion)
          L.readFile
      let cabalFile = getCabalFile pkgName pkgVersion bls
      return (cabalFile, versionSet)


-- | Download 'packages/deprecated.json' file from Hackage, parse it and save it
-- in the repositories under supplied names. Format "yaml" or "json" is guessed
-- from the file extension.
saveDeprecated :: [(GitRepository, FilePath)] -> IO ()
saveDeprecated repos = do
  deprecatedJsonReq <- parseRequest hackageDeprecatedUrl
  bs <- httpSink deprecatedJsonReq $ const foldC
  deps <- either throwM return $ decodeEither' bs :: IO [Deprecation]
  forM_
    repos
    (\(repo, filename) -> do
       let writeAs ext
             | ext `elem` [".yaml", ".yml"] -- save as YAML
              = repoFileWriter repo filename (`encodeFile` deps)
             | ext == ".json" -- save as JSON
              = repoFileWriter repo filename (`L.writeFile` A.encode deps)
             | otherwise -- save the raw downloaded bytestring
              = repoFileWriter repo filename (`S.writeFile` bs)
       writeAs (toLower $ pack $ takeExtension filename))

type PreferredInfo = Map PackageName VersionRange

loadPreferredInfo :: IO PreferredInfo
loadPreferredInfo = do
  preferredReq <- parseRequest hackagePreferredUrl
  bs <- httpSink preferredReq $ const foldC
  singletons <- mapM parse $ S8.lines bs
  return $ Map.unions singletons
  where
    parse bs
      | "--" `S.isPrefixOf` bs = return Map.empty
      | S.null bs = return Map.empty
      | otherwise = do
        let (name', range') = break (== ' ') $ S8.unpack bs
        case (,) <$> parseDistText name' <*> parseDistText range' of
          Nothing -> error $ "Invalid preferred info line: " ++ show bs
          Just (name, range) -> return $ Map.singleton name range

checkPreferred :: PreferredInfo -> PackageName -> Version -> Bool
checkPreferred preferred name version =
  case Map.lookup name preferred of
    Nothing -> True
    Just range -> version `withinRange` range

updatePackage
  :: MonadIO m
  => GitRepository -> (CabalFileEntry, Set Version) -> m ()
updatePackage metadataRepo (cfe, allVersions) =
  liftIO $ withSystemTempFile "sdist.tar.gz" updatePackage'
  where
    updatePackage' sdistFP sdistH = do
      epi <- repoFileReader metadataRepo fp decodeFileEither
      case epi of
        Right pi
          | thehash == piHash pi && -- if cabal file changed or preferred
           -- version list has been updated
              allVersions == piAllVersions pi -> return ()
        _ -> do
          putStrLn $
            concat
              ["Loading ", renderDistText name, "-", renderDistText version]
          gpd <-
            case mgpd of
              ParseFailed pe -> error $ show (name, version, pe)
              ParseOk _ gpd -> return gpd
          let pd = packageDescription gpd
          when (package pd /= PackageIdentifier name version) $
            error $ show ("mismatch" :: String, name, version, package pd)
          let name'' = renderDistText name
              version' = renderDistText version
          let url =
                concat
                  [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
                  , name''
                  , "-"
                  , version'
                  , ".tar.gz"
                  ]
          req <- parseRequest url
          res <- httpSink req $ \res -> sinkHandle sdistH >> return res
          hClose sdistH
          if getResponseStatus res == status200
            then do
              (desc, desct, cl, clt) <-
                runResourceT $
                sourceTarFile sdistFP $$
                CL.fold goEntry (pack $ description pd, "haddock", "", "")
              putStrLn $ "Updating Metadata for package: " ++ name'
              do createDirectoryIfMissing True $ takeDirectory fp
                 let checkCond = getCheckCond gpd
                     getDeps' = getDeps checkCond
                 void $ tryAny $ removeFile fp
                 repoFileWriter
                   metadataRepo
                   fp
                   (`encodeFile` PackageInfo
                                 { piLatest = version
                                 , piHash = thehash
                                 , piAllVersions = allVersions
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
                                 , piLicenseName =
                                   pack $ renderDistText $ license pd
                                 })
            else putStrLn $ "Skipping: " ++ url
          return ()
    name = cfeName cfe
    version = cfeVersion cfe
    lbs = cfeRaw cfe
    mgpd = cfeParsed cfe
    fp =
      "packages" </> (unpack $ toLower $ pack $ take 2 $ name' ++ "XX") </> name' <.>
      "yaml"
    name' = renderDistText name
    thehash = unDigest SHA256 $ hashlazy lbs
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
