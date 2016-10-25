{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stackage.Package.Locations where

import Stackage.Package.Git

hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"

hackageDeprecatedUrl :: String
hackageDeprecatedUrl = hackageBaseUrl ++ "/packages/deprecated.json"

mirrorFPComplete :: String
mirrorFPComplete = "https://s3.amazonaws.com/hackage.fpcomplete.com"

data Repositories = Repositories
  { allCabalFiles :: GitRepository
  , allCabalHashes :: GitRepository
  , allCabalMetadata :: GitRepository
  }

withRepositories :: (GitInfo, GitInfo, GitInfo)
                 -> (Repositories -> IO a)
                 -> IO a
withRepositories (filesInfo, hashesInfo, metadataInfo) action = do
  withRepository
    filesInfo
    (\filesRepo ->
        withRepository
          hashesInfo
          (\hashesRepo ->
              withRepository
                metadataInfo
                (\metadataRepo -> do
                   action $ Repositories filesRepo hashesRepo metadataRepo)))
