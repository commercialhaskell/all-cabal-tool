{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Stackage.Package.IndexConduit
  ( sourceTarFile
  , parseDistText
  , renderDistText
  , getCabalFilePath
  , getCabalFile
  , getPackageFullName
  , indexFileEntryConduit
  , sourceEntries
  , IndexFile(..)
  , IndexFileEntry(..)
  , HackageHashes(..)
  , CabalFile
  , HackageHashesFile
  ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import Control.Exception (Exception)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (MonadResource, MonadThrow(throwM))
import Data.Aeson as A
import Data.Aeson.Types as A hiding (parse)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit -- (Producer, bracketP, yield, (=$=))
import qualified Data.Conduit.List as CL
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Version (Version)
import Data.Word (Word64)
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageName)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse
       (ParseResult, parsePackageDescription)
import Distribution.Text (disp, parse)
import Distribution.Version (VersionRange)
import qualified Distribution.Text
import System.FilePath ((</>), (<.>))
import System.IO (IOMode(ReadMode), hClose, openBinaryFile)
import Text.PrettyPrint (render)

sourceTarFile
  :: MonadResource m
  => Bool -> FilePath -> Producer m Tar.Entry
sourceTarFile toUngzip fp = do
  bracketP (openBinaryFile fp ReadMode) hClose $
    \h -> do
      lbs <- liftIO $ L.hGetContents h
      sourceEntries $ Tar.read $ ungzip' lbs
  where
    ungzip'
      | toUngzip = decompress
      | otherwise = id

sourceEntries
  :: (MonadThrow m, Exception e)
  => Tar.Entries e -> ConduitM i Tar.Entry m ()
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
  { hHashes :: Map T.Text T.Text
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
decodeHackageHashes (renderDistText -> pkgName) (renderDistText -> pkgVersion) lbs = do
  val <- A.eitherDecode lbs
  A.parseEither (withObject "Package hash values from Hackage" hashesParser) val
  where
    targetKey = concat ["<repo>/package/", pkgName, "-", pkgVersion, ".tar.gz"]
    hashesParser obj = do
      signed <- obj .: "signed"
      targets <- signed .: "targets"
      target <- targets .: T.pack targetKey
      parseJSON target

type CabalFile = IndexFile (ParseResult GenericPackageDescription)

type HackageHashesFile = IndexFile (Either String HackageHashes)

-- | Versions file can be empty, which means preference list was cleared.
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
    TL.unpack $ dropBOM $ decodeUtf8With lenientDecode lbs
  }
-- https://github.com/haskell/hackage-server/issues/351
  where
    dropBOM t = fromMaybe t $ TL.stripPrefix (TL.pack "\xFEFF") t

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
            guard ('/' `notElem` fName)
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
