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
import Data.Conduit.Lazy (lazyConsume, MonadActive)
import Data.Conduit.Zlib
--import Data.IORef (modifyIORef')
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

#if MIN_VERSION_http_conduit(2,2,1)
import Network.HTTP.Simple (httpSource)
#else
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H

-- | For future compatibility, copied here from http-conduit-2.2.1
httpSource
  :: (MonadResource m, MonadIO n)
  => Request
  -> (Response (ConduitM i ByteString n ()) -> ConduitM i o m r)
  -> ConduitM i o m r
httpSource req withRes = do
  man <- liftIO H.getGlobalManager
  bracketP
    (H.responseOpen req man)
    H.responseClose
    (withRes . fmap bodyReaderSource)
#endif

data StopException = StopException deriving Show

instance Exception StopException

-- | Download a tarball from a webserver, decompress, parse it and handle it
-- using a provided `Sink`. Using a conditional function it is possible to
-- prevent a tarball from being downloaded, for instance in such a case when an
-- unexpected response status was received, in which case `Nothing` will be
-- returned. That function also allows to return any value that depends on a
-- `Response`.
httpTarballSink
  :: (MonadActive m, MonadCatch m, MonadResource m, MonadBaseControl IO m)
  => Request -- ^ Request to the tarball file.
  -> Bool -- ^ Is the tarball gzipped?
  -> Sink Tar.Entry m b -- ^ The sink of how entries in the tar file should be
     -- processed.
  -> (Response () -> (Maybe a, Bool)) -- ^ This function allows to return a
     -- value as a part of the result of `httpTarballSink`, as well as instruct
     -- if further processing of the response should continue or not.
  -> m (Maybe a, Maybe b)
httpTarballSink req isCompressed tarSink onResp = do
  retRef <- liftIO $ newIORef Nothing
  let src =
        httpSource req $
        \res -> do
          let (val, hasFuture) = onResp $ fmap (const ()) res
          hasFutureStrict <- liftIO $ atomicModifyIORef' retRef (const (val, hasFuture))
          unless hasFutureStrict $ throwM StopException
          if isCompressed
            then responseBody res =$= ungzip
            else responseBody res
  catch
    (do tarChunks <- lazyConsume src
        val <- liftIO $ (tarChunks `seq` readIORef retRef)
        result <- (sourceEntries $ Tar.read $ L.fromChunks tarChunks) $$ tarSink
        return (val, Just result)
    )
    (\(_ :: StopException) -> do
        val <- liftIO $ readIORef retRef
        return (val, Nothing))


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
  , ifParsed :: p
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

getCabalFile :: PackageName -> Version -> L.ByteString -> CabalFile
getCabalFile pkgName pkgVersion lbs =
  IndexFile
  { ifPackageName = pkgName
  , ifPackageVersion = Just pkgVersion
  , ifFileName = renderDistText pkgName <.> "cabal"
  , ifPath = getCabalFilePath pkgName pkgVersion
  , ifRaw = lbs
  , ifParsed =
    parsePackageDescription $
    unpack $ dropBOM $ decodeUtf8With lenientDecode lbs
  }
-- https://github.com/haskell/hackage-server/issues/351
  where
    dropBOM t = fromMaybe t $ TL.stripPrefix (pack "\xFEFF") t

indexFileEntryConduit
  :: Monad m
  => Conduit Tar.Entry m IndexFileEntry
indexFileEntryConduit = CL.mapMaybe getIndexFileEntry
  where
    getIndexFileEntry e@(Tar.entryContent -> Tar.NormalFile lbs _) =
      case (toPkgVer $ Tar.entryPath e) of
        Just (pkgName, Nothing, fileName@"preferred-versions") ->
          Just $
          PreferredVersionsEntry $
          IndexFile
          { ifPackageName = pkgName
          , ifPackageVersion = Nothing
          , ifFileName = fileName
          , ifPath = Tar.entryPath e
          , ifRaw = lbs
          , ifParsed = pkgVersionRange
          }
          where (pkgNameStr, range) = break (== ' ') $ L8.unpack lbs
                pkgVersionRange = do
                  pkgVersionRange' <- parseDistText range
                  pkgName' <- parseDistText pkgNameStr
                  guard (pkgName == pkgName')
                  Just pkgVersionRange'
        Just (pkgName, Just pkgVersion, fileName@"package.json") ->
          Just $
          HashesFileEntry $
          IndexFile
          { ifPackageName = pkgName
          , ifPackageVersion = Just pkgVersion
          , ifFileName = fileName
          , ifPath = Tar.entryPath e
          , ifRaw = lbs
          , ifParsed = decodeHackageHashes pkgName pkgVersion lbs
          }
        Just (pkgName, Just pkgVersion, _)
          | getCabalFilePath pkgName pkgVersion == Tar.entryPath e ->
            Just $ CabalFileEntry $ getCabalFile pkgName pkgVersion lbs
        Just (pkgName, mpkgVersion, fileName) ->
          Just $
          UnrecognizedEntry $
          IndexFile
          { ifPackageName = pkgName
          , ifPackageVersion = mpkgVersion
          , ifFileName = fileName
          , ifPath = Tar.entryPath e
          , ifRaw = lbs
          , ifParsed = ()
          }
        _ -> Nothing
    getIndexFileEntry _ = Nothing
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
