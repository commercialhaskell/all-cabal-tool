{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stackage.Package.Update where

import ClassyPrelude.Conduit
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson as A (encode)
import qualified Data.Conduit.List as CL
import Data.Yaml as Y (encodeFile)
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
              = repoFileWriter repo filename (`Y.encodeFile` deprecated)
             | otherwise -- save as JSON
              = repoFileWriter repo filename (`L.writeFile` A.encode deprecated)
       writeAs (toLower $ takeExtension filename))

-- | Saves '.cabal' files together with 'preferred-version', but ignores
-- 'package.json'
entryUpdateFile
  :: MonadIO m => GitRepository -> IndexFileEntry -> m ()
entryUpdateFile allCabalRepo (CabalFileEntry IndexFile {..}) = do
  liftIO $ repoFileWriter allCabalRepo ifPath (`L.writeFile` ifRaw)
entryUpdateFile allCabalRepo (PreferredVersionsEntry IndexFile {..}) = do
  liftIO $ repoFileWriter allCabalRepo ifPath (`L.writeFile` ifRaw)
entryUpdateFile _ _ = return ()

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
  packageVersions <-
    indexFileEntryConduit =$=
    (getZipSink
       (ZipSink (CL.mapM_ (entryUpdateFile allCabalFiles)) *>
        ZipSink (CL.mapM_ (entryUpdateFile allCabalHashes)) *>
        ZipSink (CL.mapM_ (entryUpdateHashes allCabalHashes)) *>
        ZipSink sinkPackageVersions))
  liftIO $ updateMetadata allCabalMetadata allCabalFiles packageVersions
