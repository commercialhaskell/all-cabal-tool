{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Update where

import Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.IO (hSetBinaryMode, hClose)
import System.IO.Temp (withSystemTempFile)

import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Network.HTTP.Simple (parseRequest, httpSink)

import Stackage.Package.IndexConduit
import Stackage.Package.Locations
import Stackage.Package.Metadata.Update
import Stackage.Package.Hashes



updateCabalFiles
  :: MonadIO m
  => Repositories -> Conduit CabalFileEntry m CabalFileEntry
updateCabalFiles Repositories {..} = CL.mapM handleFileEntry where
  handleFileEntry entry@(CabalFileEntry {..}) = do
    liftIO $ repoFileWriter allCabalFiles cfePath (`BL.writeFile` cfeRaw)
    return entry


updateCabalHashes
  :: MonadIO m
  => Repositories -> Conduit CabalFileEntry m CabalFileEntry
updateCabalHashes Repositories {..} = CL.mapM handleFileEntry where
  handleFileEntry entry@(CabalFileEntry {..}) = do
    liftIO $ repoFileWriter allCabalHashes cfePath (`BL.writeFile` cfeRaw)
    liftIO $ handleEntry allCabalHashes entry
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
  

allCabalUpdate :: String -> IO ()
allCabalUpdate indexUrl = do
  repos <- getRepos
  indexReq <- parseRequest indexUrl
  withSystemTempFile "01-index.tar" $
    \indexFP indexH -> do
      hSetBinaryMode indexH True
      -- Download and uncompress the index file
      httpSink indexReq $ const $ ungzip =$= sinkHandle indexH
      hClose indexH
      -- Handle some Hackage meta information.
      saveDeprecated [ (allCabalHashes repos, "deprecated.json")
                     , (allCabalMetadata repos, "deprecated.yaml") ]
      preferredInfo <- loadPreferredInfo
      -- Iterate over all cabal files.
      packageVersionsMap <-
        runResourceT $
        sourceAllCabalFiles indexFP =$= updateCabalFiles repos =$=
        updateCabalHashes repos $$
        sinkPackageVersions preferredInfo
      updateMetadata repos packageVersionsMap
      return ()
