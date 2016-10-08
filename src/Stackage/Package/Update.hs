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
  handleFileEntry entry = do
    --liftIO $ putStrLn $ "Writing all-cabal-files: " ++ cfePath
    --liftIO $ repoFileWriter allCabalFiles cfePath (`L.writeFile` cfeRaw)
    return entry


updateCabalHashes
  :: M env m
  => Repositories -> Conduit IndexFileEntry m IndexFileEntry
updateCabalHashes repos = CL.mapM handleFileEntry where
  handleFileEntry entry = do
    handleEntry (allCabalHashes repos) entry
    --liftIO $ putStrLn $ "Writing all-cabal-hashes: " ++ cfePath
    --liftIO $ repoFileWriter allCabalHashes cfePath (`L.writeFile` cfeRaw)
    --liftIO $ putStrLn $ "Handling entry all-cabal-hashes for: " ++ cfePath
    --liftIO $ handleEntry allCabalHashes entry
    return entry





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
  packageVersions <- sourceEntries entries =$= indexFileEntryConduit =$=
                     updateCabalHashes repos $$ -- CL.sinkNull
                     sinkPackageVersions
  updateMetadata repos packageVersions
  return ()


{-
allCabalUpdate indexUrl = do
  setCurrentDirectory "/home/lehins/github/all-cabal/all-cabal-hashes"
  repos <- getRepos
  return ()
  withManager $
    do withIndexFile indexUrl $
         \tarball -> do
           entries <- Tar.read . L.fromChunks <$> lazyConsume tarball
           --sourceEntries entries =$= CL.mapM handleEntry' $$ CL.sinkNull
           sourceEntries entries =$= cabalFileConduit =$=
             CL.mapM (handleEntry (allCabalHashes repos)) $$
             CL.sinkNull




  indexReq <- parseRequest indexUrl
  withSystemTempFile "01-index.tar" $
    \indexFP indexH -> do
      hSetBinaryMode indexH True
      -- Download and uncompress the index file
      httpSink indexReq $ const $ ungzip =$= sinkHandle indexH
      hClose indexH
      -- Handle some Hackage meta information.
      --saveDeprecated [ (allCabalHashes repos, "deprecated.json")
      --               , (allCabalMetadata repos, "deprecated.yaml") ]
      --preferredInfo <- loadPreferredInfo
      -- Iterate over all cabal files.
      packageVersionsMap <-
        runResourceT $
        sourceAllCabalFiles indexFP =$= -- updateCabalFiles repos =$=
        updateCabalHashes repos $$ CL.sinkNull
        -- sinkPackageVersions preferredInfo
      --updateMetadata repos packageVersionsMap
      return ()
  -}

withIndexFile' :: M env m
          => String
          -> (Source m S.ByteString -> m a)
          -> m a
withIndexFile' indexUrl inner = do
  indexReq <- parseRequest indexUrl
  withResponse indexReq
    $ \res -> inner $ responseBody res =$= ungzip

