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
  , httpTarballSink
  , IndexFile(..)
  , Cabal(..)
  , Versions(..)
  , HackagePackage(..)
  , IndexEntry(..)
  , HackageHashes(..)
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
import Data.Foldable (msum)
import Data.Version (Version)
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageName)
import Distribution.Text (disp, parse)
import Distribution.Version (VersionRange, anyVersion)
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
      target <- targets .: pack targetKey
      parseJSON target


getCabalFilePath :: PackageName -> Version -> FilePath
getCabalFilePath (renderDistText -> pkgName) (renderDistText -> pkgVersion) =
  pkgName </> pkgVersion </> pkgName <.> "cabal"

-- | A conduit that converts every tar entry of interest into `IndexEntry`.
indexFileEntryConduit
  :: (MonadBase base m, PrimMonad base, MonadThrow m)
  => Conduit Tar.Entry m IndexEntry
indexFileEntryConduit = CL.mapMaybeM getIndexFileEntry
  where
    getIndexFileEntry e@(Tar.entryContent -> Tar.NormalFile lbs sz) = do
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
