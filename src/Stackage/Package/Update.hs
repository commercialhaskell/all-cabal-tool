{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Stackage.Package.Update where

import ClassyPrelude.Conduit
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson as A (encode)
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import Data.Yaml as Y (encode)
import Network.HTTP.Simple (parseRequest, httpJSONEither, getResponseBody)

import Stackage.Package.IndexConduit
import Stackage.Package.Locations
import Stackage.Package.Metadata
import Stackage.Package.Hashes
import System.FilePath (takeExtension)


-- | Download '<repo>/packages/deprecated.json' file from Hackage, parse it and save it
-- in the repositories under supplied names. Format "yaml" or "json" is guessed
-- from the file extension.
saveDeprecated :: [(GitRepository, FilePath)] -> IO ()
saveDeprecated repos = do
  deprecatedJsonReq <- parseRequest hackageDeprecatedUrl
  edeprecated <- getResponseBody <$> httpJSONEither deprecatedJsonReq
  deprecated <- either throwM return edeprecated :: IO [Deprecation]
  forM_
    repos
    (\(repo, filename) -> do
       let writeAs ext
             | ext `elem` [".yaml", ".yml"] -- save as YAML
              = repoWriteFile_ repo filename . L.fromStrict $ Y.encode deprecated
             | otherwise -- save as JSON
              = repoWriteFile_ repo filename (A.encode deprecated)
       writeAs (toLower $ takeExtension filename))


-- | Saves '.cabal' files together with 'preferred-version', but ignores
-- 'package.json'
entryUpdateFile
  :: MonadIO m => GitRepository -> IndexFileEntry -> m ()
entryUpdateFile allCabalRepo (CabalFileEntry IndexFile {..}) = do
  liftIO $ repoWriteFile_ allCabalRepo ifPath ifRaw
entryUpdateFile allCabalRepo (PreferredVersionsEntry IndexFile {..}) = do
  liftIO $ repoWriteFile_ allCabalRepo ifPath ifRaw
entryUpdateFile _ _ = return ()


keepNewestEntryMap
  :: Map FilePath Tar.Entry -> Tar.Entry -> Map FilePath Tar.Entry
keepNewestEntryMap entriesMap entry@(Tar.entryContent -> Tar.NormalFile {}) =
  Map.insert (Tar.entryPath entry) entry entriesMap
keepNewestEntryMap entriesMap _ = entriesMap


-- | Main `Sink` that uses entries from the 00-index.tar.gz file to update all
-- relevant files in all three repos.
allCabalUpdate
  :: (MonadIO m, MonadMask m)
  => Repositories -> Sink Tar.Entry m ()
allCabalUpdate Repositories {..} = do
  liftIO $
    saveDeprecated
      [ (allCabalHashes, "deprecated.json")
      , (allCabalMetadata, "deprecated.yaml")
      ]
  newestFileEntriesMap <- CL.fold keepNewestEntryMap Map.empty
  packageVersions <-
    CL.sourceList (Map.elems newestFileEntriesMap) =$= indexFileEntryConduit =$=
    (getZipSink
       (ZipSink (CL.mapM_ (entryUpdateFile allCabalFiles)) *>
        ZipSink (CL.mapM_ (entryUpdateFile allCabalHashes)) *>
        ZipSink (CL.mapM_ (entryUpdateHashes allCabalHashes)) *>
        ZipSink sinkPackageVersions))
  liftIO $ updateMetadata allCabalMetadata newestFileEntriesMap packageVersions
