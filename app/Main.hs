{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import ClassyPrelude.Conduit hiding ((<>))
import Data.Conduit.Lazy (MonadActive)
import Data.Conduit.Zlib (gzip)
import qualified Data.ByteString.Char8 as S8 (pack)
import Control.Lens (set)
import Control.Monad (msum)
import Control.Monad.Trans.AWS (trying, _Error)
import Network.AWS
       (Credentials(Discover, FromKeys), AccessKey(..), SecretKey(..),
        Region(NorthVirginia), newEnv, runAWS, send)
import Network.AWS.S3
       (ObjectCannedACL(OPublicRead), BucketName(BucketName),
        ObjectKey(ObjectKey), poACL, putObject)
import Network.AWS.Data.Body (toBody)
import Network.HTTP.Simple
       (Request, parseRequest, addRequestHeader, getResponseStatus,
        getResponseStatusCode, getResponseHeader)
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.IO.Temp (withSystemTempDirectory)

import Stackage.Package.Update
import Stackage.Package.Locations
import Stackage.Package.IndexConduit


-- | Commit and push all changes to the repositories.
pushRepos :: Repositories -- ^ All three repositories
          -> ByteString -- ^ Commit and tag message.
          -> IO Repositories
pushRepos Repositories {..} message = do
  let repos = [allCabalFiles, allCabalHashes, allCabalMetadata]
  repos' <-
    forM repos $
    \repo@GitRepository {..} -> do
      repo' <- repoCommit repo message
      run
        repoLocalPath
        "git"
        ["push", repoAddress, concat [repoBranch, ":", repoBranch]]
      -- Tag newly created commit with the same message.
      repoTag repo message
      run repoLocalPath "git" ["push", repoAddress, "--tags", "--force"]
      return repo'
  let [allCabalFiles', allCabalHashes', allCabalMetadata'] =
        zipWith fromMaybe repos repos'
  return $
    Repositories
    { allCabalFiles = allCabalFiles'
    , allCabalHashes = allCabalHashes'
    , allCabalMetadata = allCabalMetadata'
    }
    


getRepos :: FilePath -> String -> GitUser -> IO Repositories
getRepos path account gitUser = do
  let host = "github.com"      
  allCabalFiles <-
    ensureGitRepository host account gitUser "all-cabal-files" "hackage" path
  allCabalHashes <-
    ensureGitRepository host account gitUser "all-cabal-hashes" "hackage" path
  allCabalMetadata <-
    ensureGitRepository host account gitUser "all-cabal-metadata" "master" path
  return
    Repositories
    { allCabalFiles = allCabalFiles {repoTagName = Just "current-hackage" }
    , allCabalHashes = allCabalHashes {repoTagName = Just "current-hackage" }
    , allCabalMetadata = allCabalMetadata
    }
  

-- | Upload an oldstyle '00-index.tar.gz' (i.e. without package.json files) to
-- an S3 bucket.
updateIndex00 :: Credentials -> BucketName -> GitRepository -> IO ()
updateIndex00 awsCreds bucketName GitRepository {repoTagName = Just tag
                                       ,..} = do
  env <- newEnv NorthVirginia awsCreds
  withSystemTempDirectory
    "00-index"
    (\tmpDir -> do
       let indexFP = tmpDir </> "00-index.tar"
       run
         repoLocalPath
         "git"
         ["archive", tag, "--format", "tar", "-o", indexFP]
       index00 <- runResourceT $ (sourceFile indexFP =$= gzip $$ foldC)
       let key = ObjectKey "00-index.tar.gz"
           po =
             set poACL (Just OPublicRead) $
             putObject bucketName key (toBody index00)
       eres <- runResourceT $ runAWS env $ trying _Error $ send po
       case eres of
         Left e -> error $ show (key, e)
         Right _ -> putStrLn "Success")
updateIndex00 _ _ _ = return ()


processIndexUpdate
  :: (MonadActive m, MonadIO m, MonadMask m, MonadBaseControl IO m)
  => Repositories
  -> Request -- ^ Request that should be processed
  -> Maybe ByteString -- ^ Previous Etag, so we can check if the content has changed.
  -> m (Bool, Maybe ByteString)
processIndexUpdate repos indexReq mlastEtag = do
  let indexReqWithEtag = maybe id (addRequestHeader "if-none-match") mlastEtag indexReq
  httpTarballSink
    indexReqWithEtag
    True
    (\res ->
        case getResponseStatusCode res of
          200 -> (True, listToMaybe $ getResponseHeader "etag" res) <$ allCabalUpdate repos
          304 -> return (False, mlastEtag)
          _ -> error $ "Unexpected status: " ++ show (getResponseStatus res))



type AWSInfo = (BucketName, Maybe Credentials)

data Options = Options
               { oUsername :: String
               , oEmail :: String
               , oGPGSign :: String
               , oLocalPath :: Maybe FilePath -- default $HOME
               , oDelay :: Maybe Int -- default 60 seconds
               , oS3Bucket :: Maybe BucketName
               , oAwsCredentials :: Credentials
               }


awsCredentialsParser :: Parser Credentials
awsCredentialsParser =
  FromKeys <$>
  ((AccessKey . S8.pack) <$>
   strOption
     (long "aws-access-key" <>
      help
        ("Access key for uploading 00-index.tar.gz " ++
         "(Default is $AWS_ACCESS_KEY_ID environment variable)"))) <*>
  ((SecretKey . S8.pack) <$>
   strOption
     (long "aws-secret-key" <>
      help
        ("Secret key for uploading 00-index.tar.gz " ++
         "(Default is $AWS_SECRET_KEY_ID environment variable)")))
  <|> pure Discover


optionsParser :: Parser Options
optionsParser =
  Options <$>
  (strOption (long "username" <> help "Name of the user for git commits")) <*>
  (strOption (long "email" <> help "Email of the user for git commits")) <*>
  (strOption
     (long "gpg-sign" <> help "Public GPG key of the user for git commits")) <*>
  (optional
     (strOption
        (long "path" <>
         help
           ("Path where all-cabal-* repositories are/will be. " ++
            "(Default $HOME environment variable)")))) <*>
  (optional
     (option
        auto
        (long "delay" <>
         help
           ("Delay in seconds before next check for a new version " ++
            "of 01-index.tar.gz file")))) <*>
  (optional ((BucketName . pack) <$> 
     (strOption
        (long "s3-bucket" <>
         help
           ("Access key for uploading 00-index.tar.gz. " ++
            "If none, uploading will be skipped. " ++
            "(Default is $S3_BUCKET environment variable)"))))) <*>
  awsCredentialsParser <*
  abortOption ShowHelpText (long "help" <> help "Display this help text.")



main :: IO ()
main = do
  Options {..} <- execParser (info optionsParser fullDesc)
  localPath <- maybe (getEnv "HOME") return oLocalPath
  eS3Bucket <- lookupEnv "S3_BUCKET"
  let gitAccount = "lehins" -- "commercialhaskell"
      delay = fromMaybe 60 oDelay * 1000000
      ms3Bucket = msum [oS3Bucket, (BucketName . pack) <$> eS3Bucket]
      gitUser =
        GitUser
        { userName = oUsername
        , userEmail = oEmail
        , userGPG = oGPGSign
        }
      getCommitMessage = do
        utcTime <- formatTime defaultTimeLocale "%FT%TZ" <$> getCurrentTime
        return $ fromString $ "Update from Hackage at " ++ utcTime
  when (isNothing ms3Bucket) $
    putStrLn
      "WARNING: No s3-bucket is provided. Uploading of 00-index.tar.gz will be disabled."
  indexReq <- parseRequest $ mirrorFPComplete ++ "/01-index.tar.gz"
  gitRepositories <- getRepos localPath gitAccount gitUser
  let loop repos mlastEtag = do
        putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
        commitMessage <- getCommitMessage
        (updated, mnewEtag) <-
          catchAnyDeep
            (processIndexUpdate repos indexReq mlastEtag)
            (\e -> do
               hPutStrLn stderr $
                 "ERROR: Received an unexpected exception while updating the repositories: " ++
                 show e
               return (False, mlastEtag))
        updatedRepos <-
          if updated && False
            then do
              repos' <- pushRepos repos commitMessage
              case ms3Bucket of
                Just s3Bucket ->
                  updateIndex00 oAwsCredentials s3Bucket (allCabalFiles repos)
                _ -> return ()
              return repos'
            else do
              return repos
        threadDelay delay
        loop updatedRepos mnewEtag
  loop gitRepositories Nothing
