{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Package.IndexConduit
  ( parseDistText
  , renderDistText
  , getCabalFilePath
  , getPackageFullName
  , indexFileEntryConduit
  , sourceEntries
  , localTarballSink
  , IndexFile(..)
  , Cabal(..)
  , Versions(..)
  , HackagePackage(..)
  , IndexEntry(..)
  , HackageHashes(..)
  ) where

import ClassyPrelude.Conduit
import Control.Monad.Fail
import qualified Codec.Archive.Tar as Tar
import Data.Aeson as A
import Data.Aeson.Types as A hiding (parse)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Codec.Compression.GZip as GZip
import qualified Data.Conduit.List as CL
import Data.Foldable (msum)
import Distribution.Version (Version)
import Distribution.Package (PackageName)
import Distribution.Version (VersionRange, anyVersion)
import qualified Distribution.Text
import Text.PrettyPrint (render)
import Stackage.Package.Git
import qualified Distribution.Pretty
import qualified Distribution.Parsec as Parsec

-- | Parse a tarball on disk and feed its entries to a `Sink`.
localTarballSink
  :: MonadUnliftIO m
  => FilePath -- ^ Path to the tarball.
  -> Bool -- ^ Is the tarball gzipped?
  -> Sink Tar.Entry m a -- ^ How entries in the tar file should be processed.
  -> m a
localTarballSink path isCompressed tarSink = do
  lbs <- liftIO $ L.readFile path
  (sourceEntries $ Tar.read $ decompress lbs) $$ tarSink
  where
    decompress
      | isCompressed = GZip.decompress
      | otherwise = id


sourceEntries
  :: (MonadIO m, Exception e)
  => Tar.Entries e -> Producer m Tar.Entry
sourceEntries Tar.Done = return ()
sourceEntries (Tar.Next e rest) = yield e >> sourceEntries rest
sourceEntries (Tar.Fail e) = throwIO e


-- | Any file from "01-index.tar.gz"
data IndexFile f = IndexFile
  { ifPackageName :: !PackageName
  , ifPath :: !FilePath
  , ifFile :: !f
  }

-- | ".cabal" file.
data Cabal = Cabal
  { cabalVersion :: !Version
  , cabalGitFile :: !GitFile
  }

-- | "preferred-versions" file.
data Versions = Versions
  { versionsPreferred :: !VersionRange
  , versionsGitFile :: !GitFile
  }

-- | "package.json" file.
data HackagePackage = HackagePackage
  { hackageHashes :: !HackageHashes
  , hackageVersion :: !Version
  }

-- |  An entry from a "01-index.tar.gz" file.
data IndexEntry
  = CabalEntry !(IndexFile Cabal)
  | PackageEntry !(IndexFile HackagePackage)
  | VersionsEntry !(IndexFile Versions)
  | UnknownEntry !FilePath


data HackageHashes = HackageHashes
  { hHashes :: !(Map Text Text)
  , hLength :: !Word64
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
      target <- targets .: fromString targetKey
      parseJSON target


getCabalFilePath :: PackageName -> Version -> FilePath
getCabalFilePath (renderDistText -> pkgName) (renderDistText -> pkgVersion) =
  pkgName </> pkgVersion </> pkgName <.> "cabal"

-- | A conduit that converts every tar entry of interest into `IndexEntry`.
indexFileEntryConduit
  :: MonadIO m
  => Conduit Tar.Entry m IndexEntry
indexFileEntryConduit = CL.mapMaybeM getIndexFileEntry
  where
    getIndexFileEntry e@(Tar.entryContent -> Tar.NormalFile lbs sz) = liftIO $ do
      case (toPkgVer $ Tar.entryPath e) of
        Just (pkgName, Nothing, "preferred-versions") ->
          case mpkgVersionRange of
            Nothing -> return $ Just $ UnknownEntry $ Tar.entryPath e
            Just pkgVersionRange -> do
              gitFile <- makeGitFile lbs (fromIntegral sz)
              return $
                Just $
                VersionsEntry $
                IndexFile
                { ifPackageName = pkgName
                , ifPath = Tar.entryPath e
                , ifFile =
                  Versions
                  { versionsPreferred = pkgVersionRange
                  , versionsGitFile = gitFile
                  }
                }
          where (pkgNameStr, range) = break (== ' ') $ L8.unpack lbs
                mpkgVersionRange =
                  msum
                    [ do guard (sz == 0)
                         Just anyVersion
                    , do pkgVersionRange' <- parseDistText range
                         pkgName' <- parseDistText pkgNameStr
                         guard (pkgName == pkgName')
                         Just pkgVersionRange'
                    ]
        Just (pkgName, Just pkgVersion, "package.json") -> do
          return $
            Just $
            PackageEntry $
            IndexFile
            { ifPackageName = pkgName
            , ifPath = Tar.entryPath e
            , ifFile =
              HackagePackage
              { hackageHashes = hashes
              , hackageVersion = pkgVersion
              }
            }
          where hashes =
                  case decodeHackageHashes pkgName pkgVersion lbs of
                    Left err ->
                      error $
                      "Stackage.Hackage.Hashes.entryUpdateHashes: There was an issue parsing: " ++
                      Tar.entryPath e ++ ". Parsing error: " ++ err
                    Right parsedHashes -> parsedHashes
        Just (pkgName, Just pkgVersion, _)
          | getCabalFilePath pkgName pkgVersion == Tar.entryPath e -> do
            gitFile <- makeGitFile lbs (fromIntegral sz)
            return $
              Just $
              CabalEntry $
              IndexFile
              { ifPackageName = pkgName
              , ifPath = Tar.entryPath e
              , ifFile =
                Cabal
                { cabalVersion = pkgVersion
                , cabalGitFile = gitFile
                }
              }
        _ -> return $ Just $ UnknownEntry $ Tar.entryPath e
    -- Filter out entries that are not actual files.
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
  :: (MonadFail m, Parsec.Parsec t)
  => String -> m t
parseDistText s =
  case Parsec.simpleParsec s of
    Just x -> pure x
    Nothing -> fail $ "Could not parse: " ++ s

renderDistText
  :: Distribution.Pretty.Pretty t
  => t -> String
renderDistText = Distribution.Pretty.prettyShow

-- | Generates 'pkgname-version' string.
getPackageFullName :: PackageName -> Version -> String
getPackageFullName pkgName pkgVersion =
  renderDistText pkgName ++ '-' : renderDistText pkgVersion
