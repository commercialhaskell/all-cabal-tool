{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import ClassyPrelude.Conduit
import Network.HTTP.Client (parseUrlThrow, responseCookieJar)
import Network.HTTP.Client.Conduit
import qualified Data.ByteString as S
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Network.HTTP.Simple
import Data.Function (fix)
import System.Environment (getEnv)
import Network.HTTP.Types (statusCode)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FilePath (dropExtension, takeDirectory, takeFileName)
import           Crypto.Hash.Conduit         (sinkHash)
import Data.Conduit.Process
import Data.Conduit.Zlib (ungzip)
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified System.IO as IO
import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              eitherDecode', encode, object,
                                              withObject, (.:), (.:?), (.=))
import           Crypto.Hash                 (HashAlgorithm, MD5 (..), Digest,
                                              SHA1 (..), SHA256 (..),
                                              SHA512 (..), Skein512_512 (..))
import Stackage.Package.Update
import Stackage.Package.Hashes

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
{-
allCabalFiles, allCabalHashes, allCabalMetadata :: FilePath
allCabalFiles = "all-cabal-files"
allCabalHashes = "all-cabal-hashes"
allCabalMetadata = "all-cabal-metadata"
-}
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


withIndexFile :: M env m
          => Request
          -> (Source m S.ByteString -> m a)
          -> m a
withIndexFile indexReq inner = do
  withResponse indexReq
    $ \res -> inner $ responseBody res =$= ungzip


{-processRequestWith
  :: M env m
  => Request -- ^ Request that should be processed
  -> Maybe ByteString -- ^ Previous Etag, so we can check if the content has changed.
  -> (Source m S.ByteString -> m ()) -- ^ How should we process the tarball
  -> m (Maybe ByteString) -- ^ Returns current Etag. -}
processRequestWith
  :: (MonadIO m, MonadIO m1, MonadBase base m1, PrimMonad base,
      MonadBaseControl IO m, MonadThrow m1) =>
     Request
     -> Maybe ByteString
     -> (ConduitM a1 ByteString m1 () -> ReaderT Manager m a)
     -> m (Maybe ByteString)
processRequestWith indexReq mlastEtag processor = do
  let indexReqWithEtag = maybe id (addRequestHeader "if-none-match")
                         mlastEtag indexReq
  withManager $ do
    withResponse indexReqWithEtag
      $ \res -> case getResponseStatusCode res of
        200 -> do
          putStrLn "Downloading new index and updating repositories."
          processor $ responseBody res =$= ungzip
          return $ listToMaybe $ getResponseHeader "etag" res
        304 -> return mlastEtag
        _ -> error $ "Unexpected status: " ++ show (getResponseStatus res)
      


main :: IO ()
main = do
  --allCabalUpdate "https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz"
  indexReq <- parseRequest $ "https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz"
  {-
  when False $ do
    ensureGitRepo allCabalFiles "hackage"
    ensureGitRepo allCabalHashes "hackage"
    ensureGitRepo allCabalMetadata "master"
    -}
  let loop mlastEtag = do
        putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
        mnewEtag <- 
          processRequestWith indexReq mlastEtag allCabalUpdate
        return ()
        --threadDelay rest
        --loop mnewEtag

  loop Nothing
