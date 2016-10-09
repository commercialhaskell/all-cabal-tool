{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Stackage.Package.Update where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Codec.Archive.Tar as Tar
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Resource (throwM)
import Data.Aeson as A (encode)
import Data.Char (toLower)
import Data.Conduit
import Data.Conduit.Lazy (lazyConsume, MonadActive)
import qualified Data.Conduit.List as CL
import Data.Yaml as Y (decodeEither', encodeFile)
import Network.HTTP.Simple (parseRequest, httpSink)

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
  bs <- httpSink deprecatedJsonReq $ const $ CL.foldMap id
  deps <- either throwM return $ Y.decodeEither' bs :: IO [Deprecation]
  forM_
    repos
    (\(repo, filename) -> do
       let writeAs ext
             | ext `elem` [".yaml", ".yml"] -- save as YAML
              = repoFileWriter repo filename (`Y.encodeFile` deps)
             | ext == ".json" -- save as JSON
              = repoFileWriter repo filename (`L.writeFile` A.encode deps)
             | otherwise -- save the raw downloaded bytestring
              = repoFileWriter repo filename (`S.writeFile` bs)
       writeAs (map toLower $ takeExtension filename))

-- | Saves '.cabal' files together with 'preferred-version', but ignores
-- 'package.json'
updateCabalFiles
  :: MonadIO m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalFiles Repositories {..} = CL.mapM handleFileEntry
  where
    handleFileEntry entry@(CabalFileEntry IndexFile {..}) = do
      liftIO $ repoFileWriter allCabalFiles ifPath (`L.writeFile` ifRaw)
      return entry
    handleFileEntry entry@(PreferredVersionsEntry IndexFile {..}) = do
      liftIO $ repoFileWriter allCabalFiles ifPath (`L.writeFile` ifRaw)
      return entry
    handleFileEntry entry = return entry


-- | Saves '.cabal' files together with 'preferred-version', plus calculates hash
-- values of packages source files.
updateCabalHashes
  :: M env m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalHashes repos = CL.mapM handleFileEntry
  where
    handleFileEntry entry = do
      entryUpdateHashes (allCabalHashes repos) entry
      return entry


-- | Figures out preferred version of the package and saves metadata about the
-- package using cabal files as well as some info from the package itself.
updateCabalMetadata
  :: M env m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalMetadata Repositories {..} =
  passthroughSink
    sinkPackageVersions
    (updateMetadata allCabalMetadata allCabalHashes)


-- | Perfom an update of files in all three repositories.
allCabalUpdate
  :: (MonadActive m, M env m)
  => Repositories -> Source m S.ByteString -> m ()
allCabalUpdate repos tarball = do
  liftIO $
    saveDeprecated
      [ (allCabalHashes repos, "deprecated.json")
      , (allCabalMetadata repos, "deprecated.yaml")
      ]
  entries <- Tar.read . L.fromChunks <$> lazyConsume tarball
  sourceEntries entries =$= indexFileEntryConduit =$= updateCabalFiles repos =$=
    updateCabalHashes repos =$=
    updateCabalMetadata repos $$
    CL.sinkNull
