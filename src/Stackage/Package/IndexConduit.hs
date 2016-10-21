{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stackage.Package.IndexConduit
  ( parseDistText
  , renderDistText
  , getCabalFilePath
  , getCabalFile
  , getPackageFullName
  , indexFileEntryConduit
  , sourceEntries
  , httpTarballSink
  , IndexFile(..)
  , IndexFileEntry(..)
  , HackageHashes(..)
  , CabalFile
  , HackageHashesFile
  ) where

import ClassyPrelude.Conduit
import qualified Codec.Archive.Tar as Tar
import Data.Aeson as A
import Data.Aeson.Types as A hiding (parse)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Conduit.List as CL
import Data.Conduit.Lazy (lazyConsume)
import Data.Conduit.Zlib
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL (stripPrefix)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Version (Version)
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageName)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse
       (ParseResult, parsePackageDescription)
import Distribution.Text (disp, parse)
import Distribution.Version (VersionRange)
import qualified Distribution.Text
import Network.HTTP.Client.Conduit
import Text.PrettyPrint (render)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H

import Stackage.Package.Git

-- | Download a tarball from a webserver, decompress, parse it and handle it
-- using a provided `Sink`. Using a conditional function it is possible to
-- prevent a tarball from being downloaded, for instance in such a case when an
-- unexpected response status was received, in which case `Nothing` will be
-- returned. That function also allows to return any value that depends on a
-- `Response`.
httpTarballSink
  :: (MonadMask m, MonadIO m)
  => Request -- ^ Request to the tarball file.
  -> Bool -- ^ Is the tarball gzipped?
  -> (Response () -> Sink Tar.Entry m a) -- ^ The sink of how entries in the tar file should be
     -- processed.
  -> m a
httpTarballSink req isCompressed tarSink = do
    man <- liftIO H.getGlobalManager
    bracket (liftIO $ H.responseOpen req man) (liftIO . H.responseClose) $ \res -> do
        let src' = bodyReaderSource $ H.responseBody res
            src =
                if isCompressed
                    then src' =$= ungzip
                    else src'
            res_ = const () <$> res
        tarChunks <- liftIO $ lazyConsume src
        (sourceEntries $ Tar.read $ L.fromChunks tarChunks) $$ tarSink res_


sourceEntries
  :: (MonadThrow m, Exception e)
  => Tar.Entries e -> Producer m Tar.Entry
sourceEntries Tar.Done = return ()
sourceEntries (Tar.Next e rest) = yield e >> sourceEntries rest
sourceEntries (Tar.Fail e) = throwM e



data IndexFile p = IndexFile
  { ifPackageName :: !PackageName
  , ifPackageVersion :: !(Maybe Version)
  , ifFileName :: !FilePath
  , ifPath :: !FilePath
  , ifRaw :: L.ByteString
  , ifGitFile :: GitFile
  , ifParsed :: p
  }

{-
data IndexFile f = IndexFile
  { ifPackageName :: !PackageName
  , ifFileName :: !FilePath
  , ifPath :: !FilePath
  , ifRaw :: L.ByteString
  , ifFile :: f
  }

data Cabal = Cabal
  { cabalVersion :: Version
  , cabalGitFile :: GitFile
  , cabalParsed :: ParseResult GenericPackageDescription
  }

data PackageHashes = PackageHashes
  { packageHashes :: HackageHashes
  , packageVersion :: Version
  }

data IndexEntry =
  CabalEntry (IndexFile Cabal)
  PackageEntry (IndexFile PackageHashes)
  VersionsEntry (IndexFile VersionRange)
  UnknownEntry (IndexFile ())
-}


makeIndexFile
  :: (Monad m)
  => PackageName
  -> Maybe Version
  -> FilePath
  -> FilePath
  -> L.ByteString
  -> p
  -> m (IndexFile p)
makeIndexFile packageName mpackageVersion fileName filePath raw p = do
  gitFile <- makeGitFileM filePath raw
  return $
    IndexFile
    { ifPackageName = packageName
    , ifPackageVersion = mpackageVersion
    , ifFileName = fileName
    , ifPath = filePath
    , ifRaw = raw
    , ifGitFile = gitFile
    , ifParsed = p
    }

data HackageHashes = HackageHashes
  { hHashes :: Map Text Text
  , hLength :: Word64
  }

instance FromJSON HackageHashes where
  parseJSON =
    withObject "Target hashes" $
    \o -> HackageHashes <$> o .: "hashes" <*> o .: "length"

decodeHackageHashes :: PackageName
                    -> Version
                    -> L8.ByteString
                    -> Either String HackageHashes
decodeHackageHashes pkgName pkgVersion lbs = do
  val <- A.eitherDecode lbs
  A.parseEither (withObject "Package hash values from Hackage" hashesParser) val
  where
    targetKey =
      concat
        ["<repo>/package/", getPackageFullName pkgName pkgVersion, ".tar.gz"]
    hashesParser obj = do
      signed <- obj .: "signed"
      targets <- signed .: "targets"
      target <- targets .: pack targetKey
      parseJSON target

type CabalFile = IndexFile (ParseResult GenericPackageDescription)

type HackageHashesFile = IndexFile (Either String HackageHashes)

-- | Versions file can be empty - `Nothing`, which means preference list was cleared.
type PreferredVersionsFile = IndexFile (Maybe VersionRange)

data IndexFileEntry
  = CabalFileEntry CabalFile
  | HashesFileEntry HackageHashesFile
  | PreferredVersionsEntry PreferredVersionsFile
  | UnrecognizedEntry (IndexFile ())

getCabalFilePath :: PackageName -> Version -> FilePath
getCabalFilePath (renderDistText -> pkgName) (renderDistText -> pkgVersion) =
  pkgName </> pkgVersion </> pkgName <.> "cabal"


getCabalFile
  :: (Monad m)
  => PackageName -> Version -> L.ByteString -> m CabalFile
getCabalFile pkgName pkgVersion lbs =
  makeIndexFile
    pkgName
    (Just pkgVersion)
    (renderDistText pkgName <.> "cabal")
    (getCabalFilePath pkgName pkgVersion)
    lbs
    (parsePackageDescription $ unpack $ dropBOM $ decodeUtf8With lenientDecode lbs)
-- https://github.com/haskell/hackage-server/issues/351
  where
    dropBOM t = fromMaybe t $ TL.stripPrefix (pack "\xFEFF") t

indexFileEntryConduit
  :: (Monad m)
  => Conduit Tar.Entry m IndexFileEntry
indexFileEntryConduit = CL.mapMaybeM getIndexFileEntry
  where
    getIndexFileEntry e@(Tar.entryContent -> Tar.NormalFile lbs _) = do
      case (toPkgVer $ Tar.entryPath e) of
        Just (pkgName, Nothing, fileName@"preferred-versions") ->
          (Just . PreferredVersionsEntry) <$>
          makeIndexFile pkgName Nothing fileName (Tar.entryPath e) lbs pkgVersionRange
          where (pkgNameStr, range) = break (== ' ') $ L8.unpack lbs
                pkgVersionRange = do
                  pkgVersionRange' <- parseDistText range
                  pkgName' <- parseDistText pkgNameStr
                  guard (pkgName == pkgName')
                  Just pkgVersionRange'
        Just (pkgName, Just pkgVersion, fileName@"package.json") ->
          (Just . HashesFileEntry) <$>
          makeIndexFile
            pkgName
            (Just pkgVersion)
            fileName
            (Tar.entryPath e)
            lbs
            (decodeHackageHashes pkgName pkgVersion lbs)
        Just (pkgName, Just pkgVersion, _)
          | getCabalFilePath pkgName pkgVersion == Tar.entryPath e ->
            (Just . CabalFileEntry) <$> getCabalFile pkgName pkgVersion lbs
        Just (pkgName, mpkgVersion, fileName) ->
          (Just . UnrecognizedEntry) <$>
          makeIndexFile pkgName mpkgVersion fileName (Tar.entryPath e) lbs ()
        _ -> return Nothing
    getIndexFileEntry _ = return Nothing
    toPkgVer s0 = do
      (pkgName', '/':s1) <- Just $ break (== '/') s0
      pkgName <- parseDistText pkgName'
      (mpkgVersion, fileName) <-
        case break (== '/') s1 of
          (fName, []) -> Just (Nothing, fName)
          (pkgVersion', '/':fName) -> do
            guard ('/' `onotElem` fName)
            pkgVersion <- parseDistText pkgVersion'
            return $ (Just pkgVersion, fName)
          _ -> Nothing
      return (pkgName, mpkgVersion, fileName)

parseDistText
  :: (Monad m, Distribution.Text.Text t)
  => String -> m t
parseDistText s =
  case map fst $ filter (null . snd) $ readP_to_S parse s of
    [x] -> return x
    _ -> fail $ "Could not parse: " ++ s

renderDistText
  :: Distribution.Text.Text t
  => t -> String
renderDistText = render . disp

-- | Generates 'pkgname-version' string.
getPackageFullName :: PackageName -> Version -> String
getPackageFullName pkgName pkgVersion =
  renderDistText pkgName ++ '-' : renderDistText pkgVersion
