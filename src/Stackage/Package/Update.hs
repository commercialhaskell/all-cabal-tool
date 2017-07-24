{-# LANGUAGE NamedFieldPuns #-}
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
import Distribution.Package (PackageName)
import Distribution.Version (Version)
import qualified Data.Conduit.List as CL
import Data.Yaml as Y (encode)
import Network.HTTP.Simple (parseRequest, httpJSONEither, getResponseBody)


import Stackage.Package.IndexConduit
import Stackage.Package.Git
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
              = repoWriteFile repo filename (L.fromStrict $ Y.encode deprecated)
             | otherwise -- save as JSON
              = repoWriteFile repo filename (A.encode deprecated)
       writeAs (toLower $ takeExtension filename))


-- | Saves '.cabal' files together with 'preferred-version', but ignores
-- 'package.json'
entryUpdateFile
  :: MonadIO m
  => (PackageName -> Version -> Bool) -> GitRepository -> IndexEntry -> m ()
entryUpdateFile hasHashes allCabalRepo (CabalEntry IndexFile {..}) =
  liftIO $ do
    when (hasHashes ifPackageName (cabalVersion ifFile)) $
      repoWriteGitFile allCabalRepo ifPath (cabalGitFile ifFile)
entryUpdateFile _ allCabalRepo (VersionsEntry IndexFile {..}) =
  liftIO $ repoWriteGitFile allCabalRepo ifPath (versionsGitFile ifFile)
entryUpdateFile _ _ _ = return ()


-- | Main validation `Sink` that uses entries from the 01-index.tar.gz file to
-- generate hashes for source files and check them against hackage's
-- package.json file. All package versions failing the validation are excluded
-- from returned map. Hash values for new packages are generated and added to
-- hashes repo.
allHashesUpdate
  :: (MonadIO m, MonadMask m, PrimMonad base, MonadBase base m)
  => Repositories -> Sink Tar.Entry m (Map PackageName (Set Version))
allHashesUpdate Repositories {..} = do
  indexFileEntryConduit =$= sinkPackageHashes allCabalHashes


-- | Main `Sink` that uses entries from the 01-index.tar.gz file to update all
-- relevant files in all three repos.
allCabalUpdate
  :: (MonadIO m, MonadMask m, PrimMonad base, MonadBase base m)
  => Repositories
  -> Map PackageName (Set Version)
  -> Sink Tar.Entry m ()
allCabalUpdate repos@Repositories {..} versionsMap = do
  liftIO $
    saveDeprecated
      [ (allCabalHashes, "deprecated.json")
      , (allCabalMetadata, "deprecated.yaml")
      ]
  -- generate and validate all hashes, while keeping only relevant files
  let hasHashes = containsHashesFor versionsMap
  packageVersions <-
    indexFileEntryConduit =$=
    (getZipSink
       (ZipSink (CL.mapM_ (entryUpdateFile hasHashes allCabalFiles)) *>
        ZipSink (CL.mapM_ (entryUpdateFile hasHashes allCabalHashes)) *>
        ZipSink sinkPackageVersions))
  updateMetadata repos versionsMap packageVersions
