{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Update where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Directory
import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import Data.Conduit.Lazy (lazyConsume, MonadActive)
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Network.HTTP.Client.Conduit
import Stackage.Package.IndexConduit
import Stackage.Package.Locations
import Stackage.Package.Metadata.Update
import Stackage.Package.Hashes

import qualified Codec.Archive.Tar as Tar
import Debug.Trace


updateCabalFiles
  :: MonadIO m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalFiles Repositories {..} = CL.mapM handleFileEntry where
  handleFileEntry entry@(CabalFileEntry IndexFile {..}) = do
    liftIO $ repoFileWriter allCabalFiles ifPath (`L.writeFile` ifRaw)
    return entry
  handleFileEntry entry = return entry


updateCabalHashes
  :: M env m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalHashes repos = CL.mapM handleFileEntry where
  handleFileEntry entry = do
    entryUpdateHashes (allCabalHashes repos) entry
    return entry


updateCabalMetadata
  :: M env m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalMetadata repos = passthroughSink sinkPackageVersions (updateMetadata repos) 
  



getRepos :: IO Repositories
getRepos = do
  let github = "github.com"
      account = "lehins" -- "commercialhaskell"
      path = "/home/lehins/github/all-cabal" -- get $HOME path
  allCabalFiles <- ensureGitRepository github account "all-cabal-files" path
  allCabalHashes <- ensureGitRepository github account "all-cabal-hashes" path
  allCabalMetadata <- ensureGitRepository github account "all-cabal-metadata" path
  return Repositories { allCabalFiles = allCabalFiles
                      , allCabalHashes = allCabalHashes
                      , allCabalMetadata = allCabalMetadata }
  

allCabalUpdate :: (MonadActive m, M env m) => Source m S.ByteString -> m ()
allCabalUpdate tarball = do
  repos <- liftIO getRepos
  entries <- Tar.read . L.fromChunks <$> lazyConsume tarball
  sourceEntries entries =$= indexFileEntryConduit =$=
    updateCabalFiles repos =$=
    updateCabalHashes repos =$=
    updateCabalMetadata repos $$ CL.sinkNull
