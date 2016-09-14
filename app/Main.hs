{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ClassyPrelude.Conduit
import Network.HTTP.Simple
import Data.Function (fix)
import System.Environment (getEnv)
import Network.HTTP.Types (statusCode)
import System.Directory (doesDirectoryExist)
import Data.Conduit.Process

-- TODO need to add some kind of verification with hackage-security,
-- once I figure out how to use that package

indexTarGz :: FilePath
indexTarGz = "00-index.tar.gz"

rest :: Int
rest = 1000000 * 60 -- one minute

run :: FilePath -> FilePath -> [String] -> IO ()
run dir cmd args = do
    putStrLn $ concat
        [ "Running in "
        , tshow dir
        , ": "
        , unwords $ map pack $ cmd : args
        ]
    withCheckedProcessCleanup
        (proc cmd args) { cwd = Just dir }
        (\Inherited Inherited Inherited -> return ())

allCabalFiles, allCabalHashes, allCabalMetadata :: FilePath
allCabalFiles = "all-cabal-files"
allCabalHashes = "all-cabal-hashes"
allCabalMetadata = "all-cabal-metadata"

ensureGitRepo :: FilePath -- ^ Dest directory
              -> String -- ^ Branch
              -> IO ()
ensureGitRepo dir branch = do
    exists <- doesDirectoryExist dir
    if exists
        then do
            run dir "git" ["clean", "-fdx"]
            run dir "git" ["remote", "set-url", "origin", url]
            run dir "git" ["fetch"]
            run dir "git" ["checkout", "origin/" ++ branch]
            run dir "git" ["branch", "-D", branch]
            run dir "git" ["checkout", "-b", branch]
            run dir "git" ["branch", "-u", "origin/" ++ branch]
            run dir "git" ["clean", "-fdx"]
        else run "." "git"
            [ "clone"
            , "--depth=1"
            , url
            , dir
            , "--branch"
            , unpack branch
            ]
    run dir "git" ["config", "core.autocrlf", "input"]
  where
    url = "git@github.com:commercialhaskell/" ++ dir

main :: IO ()
main = do
    hackageRoot <- getEnv "HACKAGE_ROOT"
    indexReq <- parseRequest $ hackageRoot ++ "/packages/00-index.tar.gz"

    ensureGitRepo allCabalFiles "hackage"
    ensureGitRepo allCabalHashes "hackage"
    ensureGitRepo allCabalMetadata "master"

    let loop mlastEtag = do
            putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
            let req = maybe id (addRequestHeader "if-none-match")
                      mlastEtag indexReq

                download = runResourceT $ httpSink req $ \res ->
                    case statusCode $ getResponseStatus res of
                        200 -> do
                            putStrLn "Downloading new index"
                            sinkFile indexTarGz
                            return ( True
                                   , listToMaybe $ getResponseHeader "etag" res
                                   )
                        304 -> return (False, mlastEtag)
                        _ -> error $ "Unexpected status: " ++
                                    show (getResponseStatus res)

            (downloaded, mnewEtag) <- runResourceT $ download `catchAny` \e -> do
                putStrLn $ "Exception occurred downloading index: " ++
                           tshow e
                return (False, mlastEtag)

            print (downloaded, mnewEtag)

            threadDelay rest
            loop mnewEtag

    loop Nothing
