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
import Data.Conduit.Process
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
import System.Environment (setEnv, getEnv, lookupEnv)
import System.Exit (ExitCode(..))
import System.Directory (doesDirectoryExist)
import System.IO.Temp (withSystemTempDirectory)

import Stackage.Package.Update
import Stackage.Package.Locations
import Stackage.Package.IndexConduit

run :: FilePath -> FilePath -> [String] -> IO ()
run dir cmd args = do
  putStrLn $
    concat ["Running in ", tshow dir, ": ", unwords $ map pack $ cmd : args]
  withCheckedProcessCleanup
    (proc cmd args)
    { cwd = Just dir
    }
    (\Inherited Inherited Inherited -> return ())


-- | Similar to `run`, but will return the process' output as a `ByteString`.
getOutput :: FilePath -> FilePath -> [String] -> IO ByteString
getOutput dir cmd args = do
  putStrLn $
    concat ["Running in ", tshow dir, ": ", unwords $ map pack $ cmd : args]
  (exitCode, out) <-
    sourceProcessWithConsumer
      (proc cmd args)
      { cwd = Just dir
      }
      foldC
  case exitCode of
    ExitSuccess -> return out
    code ->
      error $
      "Calling: " ++
      showCommandForUser cmd args ++ " produced an error " ++ show code


-- | Clones the repo if it doesn't exists locally yet, otherwise pulls from it.
ensureGitRepository
  :: String -- ^ Git provider ex. "github.com"
  -> String -- ^ Repository account ex. "commercialhaskell"
  -> GitUser -- ^ User information to be used for the commits.
  -> String -- ^ Repository name ex. "all-cabal-files"
  -> String -- ^ Repository branch ex. "master"
  -> FilePath -- ^ Location in the file system where
     -- repository should be cloned to.
  -> IO GitRepository
ensureGitRepository repoHost repoAccount gitUser repoName repoBranch repoBasePath = do
  exists <- doesDirectoryExist repoLocalPath
  if exists
    then do
      run repoLocalPath "git" ["clean", "-fdx"]
      run repoLocalPath "git" ["remote", "set-url", "origin", repoAddress]
      run repoLocalPath "git" ["fetch"]
      run repoLocalPath "git" ["checkout", "origin/" ++ repoBranch]
      run repoLocalPath "git" ["branch", "-D", repoBranch]
      run repoLocalPath "git" ["checkout", "-b", repoBranch]
      run repoLocalPath "git" ["branch", "-u", "origin/" ++ repoBranch]
      run repoLocalPath "git" ["clean", "-fdx"]
    else run
           "."
           "git"
           [ "clone"
           , "--depth=1"
           , repoAddress
           , repoLocalPath
           , "--branch"
           , repoBranch
           ]
  run repoLocalPath "git" ["config", "user.name", userName gitUser]
  run repoLocalPath "git" ["config", "user.email", userEmail gitUser]
  run repoLocalPath "git" ["config", "core.autocrlf", "input"]
  return repo
  where
    repo =
      GitRepository
      { repoAddress = repoAddress
      , repoBranch = repoBranch
      , repoUser = gitUser
      , repoTag = Nothing
      , repoLocalPath = repoLocalPath
      }
    repoLocalPath = repoBasePath </> repoName
    repoAddress =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]


-- | Commit and push all changes to the repositories.
commitRepos :: Repositories -- ^ All three repositories
            -> IO ()
commitRepos Repositories {..} = do
  let getCommitMsg = do
        utcTime <- formatTime defaultTimeLocale "%FT%TZ" <$> getCurrentTime
        return $ "Update from Hackage at " ++ utcTime
  mapM_
    (commitRepo getCommitMsg)
    [allCabalFiles, allCabalHashes, allCabalMetadata]
    


commitRepo :: IO String -> GitRepository -> IO ()
commitRepo getCommitMsg GitRepository {..} = do
  run repoLocalPath "git" ["add", "-A"]
  out <- getOutput repoLocalPath "git" ["status", "--porcelain"]
  if null out
    then putStrLn $ "Info: Nothing to commit in " ++ pack repoLocalPath
    else do
      commitMsg <- getCommitMsg
      run
        repoLocalPath
        "git"
        ["commit", "-m", commitMsg, "--gpg-sign=" ++ userGPG repoUser]
      run repoLocalPath "git" ["push", repoAddress, "HEAD:" ++ repoBranch]
      forM_
        repoTag
        (\tag -> do
           run
             repoLocalPath
             "git"
             ["tag", tag, "-u", userGPG repoUser, "-m", commitMsg, "-f"]
           run repoLocalPath "git" ["push", repoAddress, "--tags", "--force"])




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
    { allCabalFiles = allCabalFiles {repoTag = Just "current-hackage" }
    , allCabalHashes = allCabalHashes {repoTag = Just "current-hackage" }
    , allCabalMetadata = allCabalMetadata
    }
  

-- | Upload an oldstyle '00-index.tar.gz' (i.e. without package.json files) to
-- an S3 bucket.
updateIndex00 :: Credentials -> String -> GitRepository -> IO ()
updateIndex00 awsCreds bucketName GitRepository {repoTag = Just tag
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
             putObject (BucketName $ pack bucketName) key (toBody index00)
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
  (mnewEtag, msunk) <- runResourceT $ httpTarballSink
    indexReqWithEtag
    True
    (allCabalUpdate repos)
    (\res ->
        case getResponseStatusCode res of
          200 -> (listToMaybe $ getResponseHeader "etag" res, True)
          304 -> (mlastEtag, False)
          _ -> error $ "Unexpected status: " ++ show (getResponseStatus res))
  return $ case msunk of
    Just _ -> (True, mnewEtag)
    Nothing -> (False, mnewEtag)




data Options = Options
               { oUsername :: String
               , oEmail :: String
               , oGPGSign :: String
               , oLocalPath :: Maybe FilePath -- default $HOME
               , oDelay :: Maybe Int -- default 60 seconds
               , oAwsAccessKey :: Maybe String -- default $AWS_ACCESS_KEY_ID
               , oAwsSecretKey :: Maybe String -- default $AWS_SECRET_KEY_ID
               , oS3Bucket :: Maybe String -- default $S3_BUCKET
               }


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
  (optional
     (strOption
        (long "aws-access-key" <>
         help
           ("Access key for uploading 00-index.tar.gz " ++
            "(Default is $AWS_ACCESS_KEY_ID environment variable)")))) <*>
  (optional
     (strOption
        (long "aws-secret-key" <>
         help
           ("Secret key for uploading 00-index.tar.gz " ++
            "(Default is $AWS_SECRET_KEY_ID environment variable)")))) <*>
  (optional
     (strOption
        (long "s3-bucket" <>
         help
           ("Access key for uploading 00-index.tar.gz. " ++
            "If none, uploading will be skipped. " ++
            "(Default is $S3_BUCKET environment variable)")))) <*
  abortOption ShowHelpText (long "help" <> help "Display this help text.")



main :: IO ()
main = do
  Options {..} <- execParser (info optionsParser fullDesc)
  localPath <- maybe (getEnv "HOME") return oLocalPath
  eS3Bucket <- lookupEnv "S3_BUCKET"
  let gitAccount = "lehins" -- "commercialhaskell"
      delay = fromMaybe 60 oDelay * 1000000
      ms3Bucket = msum [oS3Bucket, eS3Bucket]
      gitUser =
        GitUser
        { userName = oUsername
        , userEmail = oEmail
        , userGPG = oGPGSign
        }
  case (ms3Bucket, oAwsAccessKey, oAwsSecretKey) of
    (Just _, Just accessKey, Nothing) -> setEnv "AWS_ACCESS_KEY_ID" accessKey
    (Just _, Nothing, Just secretKey) -> setEnv "AWS_SECRET_KEY_ID" secretKey
    (Nothing, _, _) ->
      putStrLn
        "WARNING: No s3-bucket is provided. Uploading of 00-index.tar.gz will be disabled."
    _ -> return ()
  indexReq <- parseRequest $ mirrorFPComplete ++ "/01-index.tar.gz"
  repos <- getRepos localPath gitAccount gitUser
  let loop mlastEtag = do
        putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
        (updated, mnewEtag) <-
          catchAnyDeep
            (processIndexUpdate repos indexReq mlastEtag)
            (\e -> do
               hPutStrLn stderr $
                 "ERROR: Received an unexpected exception while updating the repositories: " ++
                 show e
               return (False, mlastEtag))
        when
          updated
          (do commitRepos repos
              case (ms3Bucket, oAwsAccessKey, oAwsSecretKey) of
                (Just s3Bucket, Just awsAccessKey, Just awsSecretKey) ->
                  updateIndex00
                    (FromKeys
                       (AccessKey $ S8.pack awsAccessKey)
                       (SecretKey $ S8.pack awsSecretKey))
                    s3Bucket
                    (allCabalFiles repos)
                (Just s3Bucket, Nothing, Nothing) ->
                  updateIndex00 Discover s3Bucket (allCabalFiles repos)
                _ -> return ())
        threadDelay delay
        loop mnewEtag
  loop Nothing
