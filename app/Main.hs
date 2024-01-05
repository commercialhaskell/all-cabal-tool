{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import ClassyPrelude.Conduit
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as S8 (pack)
import Control.Lens (set)
import Control.Monad (msum)
import Amazonka
       (AccessKey(..), SecretKey(..), Env, EnvNoAuth,
        newEnv, send, trying, _Error, toBody, discover)
import Amazonka.Auth (fromKeys)
import Amazonka.S3.PutObject
import Amazonka.S3 (BucketName(..), ObjectKey(..), ObjectCannedACL(..))
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Simple
       (Request, parseRequest, addRequestHeader, getResponseStatus,
        getResponseStatusCode, getResponseHeader, getResponseBody, httpLBS)
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.IO (hPutStrLn)

import Stackage.Package.Update
import Stackage.Package.Locations
import Stackage.Package.Git
import Stackage.Package.IndexConduit


-- | Commit and push all changes to the repositories.
pushRepos :: Repositories -- ^ All three repositories
          -> ByteString -- ^ Message to be used for both commit and a tag.
          -> IO ()
pushRepos Repositories {..} message = do
  forM_ [allCabalFiles, allCabalHashes, allCabalMetadata] $
    \repo@GitRepository {repoInfo = GitInfo {..}} -> do
      mcommitRef <- repoCreateCommit repo message
      case mcommitRef of
        Nothing -> return ()
        Just commitRef -> do
          run
            gitLocalPath
            "git"
            ["push", gitAddress, concat [gitBranchName, ":", gitBranchName]]
          mtagRef <- repoCreateTag repo commitRef message
          case mtagRef of
            -- Tag newly created commit with the same message.
            Just _ -> run gitLocalPath "git" ["push", gitAddress, "--tags", "--force"]
            Nothing -> return ()
          return ()

getReposInfo :: FilePath -> String -> GitUser -> IO (GitInfo, GitInfo, GitInfo)
getReposInfo path account gitUser = do
  let host = "github.com"
  allCabalFilesInfo <-
    ensureRepository host account gitUser "all-cabal-files" "hackage" path
  allCabalHashesInfo <-
    ensureRepository host account gitUser "all-cabal-hashes" "hackage" path
  allCabalMetadataInfo <-
    ensureRepository host account gitUser "all-cabal-metadata" "master" path
  return
    ( allCabalFilesInfo
      { gitTagName = Just "current-hackage"
      }
    , allCabalHashesInfo
      { gitTagName = Just "current-hackage"
      }
    , allCabalMetadataInfo)

-- | Upload an oldstyle '00-index.tar.gz' (i.e. without package.json files) to
-- an S3 bucket.
updateIndex00 :: AwsDiscoverMechanism -> BucketName -> IO ()
updateIndex00 awsMech bucketName = do
  {-
  No longer works since 00-index.tar.gz is being modified. But
  thankfully we can finally securely download the file, since
  Hackage Origin has HTTPS available! See:
  https://github.com/haskell/cabal/issues/4624#issuecomment-325030373

  GitRepository {repoInfo = GitInfo {gitTagName = Just tagName
  withSystemTempDirectory
    "00-index"
    (\tmpDir -> do

       let indexFP = tmpDir </> "00-index.tar"
       run
         gitLocalPath
         "git"
         ["archive", S8.unpack tagName, "--format", "tar", "-o", indexFP]
       index00 <- runResourceT $ (sourceFile indexFP =$= gzip $$ foldC)
       -}

  env <- newEnv awsMech
  req <- parseUrlThrow "https://hackage-origin.haskell.org/packages/00-index.tar.gz"
  index00 <- getResponseBody <$> httpLBS req
  let key = ObjectKey "00-index.tar.gz"
      po =
        set putObject_acl (Just ObjectCannedACL_Public_read) $
        newPutObject bucketName key (toBody index00)
  eres <- runResourceT $ trying _Error $ send env po
  case eres of
    Left e -> error $ show (key, e)
    Right _ -> putStrLn "Success"

processIndexUpdate
  :: MonadIO m
  => Repositories
  -> Request -- ^ Request that should be processed
  -> Maybe ByteString -- ^ Previous Etag, so we can check if the content has changed.
  -> m (Bool, Maybe ByteString)
processIndexUpdate repos indexReq mLastEtag = liftIO $ do
  let indexReqWithEtag =
        maybe id (addRequestHeader "if-none-match") mLastEtag indexReq
  mValidVersionsWithEtag <-
    httpTarballSink
      indexReqWithEtag
      True
      (\res ->
         case getResponseStatusCode res of
           200 -> do
             validVersions <- allHashesUpdate repos
             return $
               Just (validVersions, listToMaybe $ getResponseHeader "etag" res)
           304 -> return Nothing
           _ -> error $ "Unexpected status: " ++ show (getResponseStatus res))
  case mValidVersionsWithEtag of
    Nothing -> return (False, mLastEtag)
    Just (validVersions, mNewEtag) ->
      httpTarballSink
        indexReqWithEtag
        True
        (\res ->
           case getResponseStatusCode res of
             200 -> (True, mNewEtag) <$ allCabalUpdate repos validVersions
             304 -> return (False, mLastEtag)
             _ -> error $ "Unexpected status: " ++ show (getResponseStatus res))



type AwsDiscoverMechanism = EnvNoAuth -> IO Env

data Options = Options
               { oUsername :: String
               , oEmail :: String
               , oGPGSign :: String
               , oLocalPath :: Maybe FilePath -- default $HOME
               , oGithubAccount :: String -- default "commercialhaskell"
               , oDelay :: Maybe Int -- default 60 seconds
               , oS3Bucket :: Maybe BucketName
               , oAwsDiscoveryMech :: AwsDiscoverMechanism
               }


awsCredentialsParser :: Parser AwsDiscoverMechanism
awsCredentialsParser =
  (\a s -> pure . fromKeys a s) <$>
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
  <|> pure discover


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
  (strOption
     (long "github-account" <> value "commercialhaskell" <>
      help
        ("Github account where all-cabal-* repos are located. " ++
         "(Default \"commercialhaskell\")"))) <*>
  (optional
     (option
        auto
        (long "delay" <>
         help
           ("Delay in seconds before next check for a new version " ++
            "of 01-index.tar.gz file")))) <*>
  (optional
     ((BucketName . pack) <$>
      (strOption
         (long "s3-bucket" <>
          help
            ("Access key for uploading 00-index.tar.gz. " ++
             "If none, uploading will be skipped. " ++
             "(Default is $S3_BUCKET environment variable)"))))) <*>
  awsCredentialsParser <*
  abortOption (ShowHelpText Nothing) (long "help" <> help "Display this help text.")



main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Options {..} <- execParser (info optionsParser fullDesc)
  localPath <- maybe (getEnv "HOME") return oLocalPath
  eS3Bucket <- lookupEnv "S3_BUCKET"
  let delay = fromMaybe 60 oDelay * 1000000
      ms3Bucket = msum [oS3Bucket, (BucketName . pack) <$> eS3Bucket]
      gitUser =
        GitUser
        { userName = S8.pack oUsername
        , userEmail = S8.pack oEmail
        , userGpgKey = Just oGPGSign
        }
      getCommitMessage = do
        utcTime <- formatTime defaultTimeLocale "%FT%TZ" <$> getCurrentTime
        return $ fromString $ "Update from Hackage at " ++ utcTime
  when (isNothing ms3Bucket) $
    putStrLn
      "WARNING: No s3-bucket is provided. Uploading of 00-index.tar.gz will be disabled."
  indexReq <- parseRequest $ mirrorFPComplete ++ "/01-index.tar.gz"
  reposInfoInit <- getReposInfo localPath oGithubAccount gitUser
  let innerLoop reposInfo mlastEtag = do
        putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
        commitMessage <- getCommitMessage
        (newInfo, mnewEtag) <- withRepositories reposInfo $ \ repos -> do
          (updated, mnewEtag) <- processIndexUpdate repos indexReq mlastEtag
          when updated $
            do pushRepos repos commitMessage
               case ms3Bucket of
                 Just s3Bucket ->
                   updateIndex00 oAwsDiscoveryMech s3Bucket
                 _ -> return ()
          return mnewEtag
        threadDelay delay
        innerLoop newInfo mnewEtag
  let outerLoop = do
        catchAny
          (innerLoop reposInfoInit Nothing)
          (\e -> do
             hPutStrLn stderr $
               "ERROR: Received an unexpected exception while updating repositories: " ++
               show e
             threadDelay delay
             outerLoop)
  outerLoop
