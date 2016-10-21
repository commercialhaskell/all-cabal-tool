{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Git.Repository where

import ClassyPrelude.Conduit
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Git hiding (Commit, Tag)
import qualified Data.Git.Types as G
import Data.Git.Repository
import qualified Data.Git.Storage.Object as G
import Data.Git.Storage.Loose
import qualified Filesystem.Path.CurrentOS as P
import Data.Conduit.Process
       (withCheckedProcessCleanup, sourceProcessWithStreams,
        Inherited(Inherited))
import ClassyPrelude.Conduit (sourceLazy, sinkLazyBuilder)
import System.Directory
import System.Exit
import System.Process (proc, cwd, showCommandForUser)

import Stackage.Package.Git.Types
import Stackage.Package.Git.Object
import Stackage.Package.Git.WorkTree

withRepository :: GitInfo -> (GitRepository -> IO a) -> IO a
withRepository info@GitInfo {..} action = do
  properRepo <- isRepo (P.decodeString gitLocalPath)
  unless properRepo $ error $ "There is no git repository in :" ++ gitLocalPath
  withRepo (P.decodeString gitLocalPath) $
    \git -> do
      branchRef <-
        maybe
          (error $
           "Cannot resolve " ++ gitBranchName ++ " for repository: " ++ gitLocalPath)
          id <$>
        resolveRevision git (fromString gitBranchName)
      rootTreeRef <-
        maybe
          (error $
           "Cannot resolve root tree for " ++
           gitBranchName ++ " for repository: " ++ gitLocalPath)
          id <$>
        resolvePath git branchRef []
      headSet git (Right (RefName gitBranchName))
      rootTree <- readWorkTree git rootTreeRef
      branchRefMVar <- newMVar branchRef
      rootTreeMVar <- newMVar rootTree
      workTreeMVar <- newMVar emptyWorkTree
      action
        GitRepository
        { repoInstance =
          GitInstance
          { gitRepo = git
          , gitBranchRef = branchRefMVar
          , gitRootTree = rootTreeMVar
          , gitWorkTree = workTreeMVar
          }
        , repoInfo = info
        }



repoReadFile :: GitRepository -> FilePath -> IO (Maybe LByteString)
repoReadFile GitRepository {repoInstance = GitInstance {..}} fp = do
  workTree <- readMVar gitWorkTree
  case lookupFile workTree fp of
    Just f -> return $ Just $ gitFileContent f
    Nothing -> do
      rootTree <- readMVar gitRootTree
      case lookupFile rootTree fp of
        Just ref -> do
          mobj <- getObject gitRepo ref True
          case mobj of
            Just (G.ObjBlob (G.Blob blob)) -> return $ Just blob
            _ -> return Nothing
        Nothing -> return Nothing


-- | Same as `readRepoFile`, but will raise an error if file cannot be found.
repoReadFile' :: GitRepository -> FilePath -> IO L.ByteString
repoReadFile' repo@GitRepository {repoInfo = GitInfo {..}} fp = do
  mfile <- repoReadFile repo fp
  case mfile of
    Just file -> return file
    Nothing ->
      error $
      "Could not find file: " ++
      fp ++ " in repository: " ++ gitLocalPath ++ " under branch: " ++ gitBranchName


repoWriteFile :: GitRepository -> GitFile -> IO ()
repoWriteFile GitRepository {repoInstance = GitInstance {..}} f =
  withMVar gitRootTree $
  \rootTree ->
     modifyMVar_ gitWorkTree $
     \workTree ->
        case lookupFile rootTree (gitFilePath f) of
          Just ref ->
            return $
            if ref == gitFileRef f
              then removeGitFile workTree (gitFilePath f)
              else insertGitFile workTree f
          Nothing -> return $ insertGitFile workTree f



--repoDeleteFile :: GitRepository -> FilePath -> IO ()
--repoDeleteFile = undefined



repoCreateCommit :: GitRepository -> ByteString -> IO (Maybe Ref)
repoCreateCommit repo@GitRepository {repoInstance = GitInstance {..}
                                    ,repoInfo = GitInfo {..}} msg = do
  mtreeRef <- flushWorkTree repo
  case mtreeRef of
    Nothing -> do
      putStrLn "Nothing to commit"
      return Nothing
    Just treeRef -> do
      modifyMVar gitBranchRef $ \branchRef -> do
        commit <- makeGitCommit treeRef [branchRef] gitUser msg
        newCommit <- case userGpgKey gitUser of
          Just key -> signCommit gitRepo key commit
          Nothing -> return commit
        commitRef <- repoWriteObject repo (Commit newCommit)
        branchWrite gitRepo (RefName gitBranchName) commitRef
        return (commitRef, Just commitRef)



--repoReadCommit :: GitRepository -> String -> IO GitCommit
--repoReadCommit = undefined



repoCreateTag :: GitRepository -> Ref -> ByteString -> IO (Maybe Ref)
repoCreateTag repo@GitRepository {repoInstance = GitInstance {..}
                                 ,repoInfo = GitInfo {gitTagName = Just tagName
                                                     ,..}} commitRef tagMessage = do
  tag <- makeGitTag commitRef gitUser tagName tagMessage
  newTag <-
    case userGpgKey gitUser of
      Just key -> signTag gitRepo key tag
      Nothing -> return tag
  Just <$> repoWriteObject repo (Tag newTag)
repoCreateTag _ _ _ = return Nothing




-- | Clones the repo if it doesn't exists locally yet, otherwise fetches and
-- creates a local branch.
ensureRepository
  :: String -- ^ Git provider ex. "github.com"
  -> String -- ^ Repository account ex. "commercialhaskell"
  -> GitUser -- ^ User information to be used for the commits.
  -> String -- ^ Repository name ex. "all-cabal-files"
  -> String -- ^ Repository branch ex. "master"
  -> FilePath -- ^ Location in the file system where
     -- repository should be cloned to.
  -> IO GitInfo
ensureRepository repoHost repoAccount gitUser repoName repoBranchName repoBasePath = do
  exists <- doesDirectoryExist repoLocalPath
  if exists
    then do
      run repoLocalPath "git" ["remote", "set-url", "origin", repoAddress]
      run repoLocalPath "git" ["fetch"]
    else run
           "."
           "git"
           [ "clone"
           , "--bare"
           , "--depth=1"
           , repoAddress
           , repoLocalPath
           , "--branch"
           , repoBranchName             
           ]
  --run repoLocalPath "git" ["config", "user.name", userName gitUser]
  --run repoLocalPath "git" ["config", "user.email", userEmail gitUser]
  run repoLocalPath "git" ["config", "core.autocrlf", "input"]
  return
    GitInfo
    { gitAddress = repoAddress
    , gitBranchName = repoBranchName
    , gitUser = gitUser
    , gitTagName = Nothing
    , gitLocalPath = repoLocalPath
    }
  where
    repoLocalPath = repoBasePath </> repoName
    repoAddress =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]



signCommit :: Git -> String -> G.Commit -> IO G.Commit
signCommit gitRepo key commit = do
  signature <- signObject gitRepo key (G.ObjCommit commit)
  -- Workaround for: https://github.com/vincenthz/hit/issues/35
  -- Otherwise should be:
  -- let signatureKey = "gpgsig"
  --     signature = L.toStrict out
  let signatureKey = L.toStrict $ L.append "gpgsig " $ L8.takeWhile (/='\n') signature
      signature' = L.toStrict $ L.tail $ L8.dropWhile (/='\n') signature
  return
    commit
    { commitExtras = commitExtras commit ++ [CommitExtra signatureKey signature']
    }


signTag :: Git -> String -> G.Tag -> IO G.Tag
signTag gitRepo key tag = do
  signature <- signObject gitRepo key (G.ObjTag tag)
  return
    tag
    { tagS = S8.intercalate "\n" [tagS tag, L.toStrict signature]
    }


signObject :: Git -> String -> G.Object -> IO L.ByteString
signObject gitRepo key obj = do
  gpgProgram <- maybe "gpg" id <$> configGet gitRepo "gpg" "program"
  let args = ["-bsau", key]
  let payload = L.tail $ L.dropWhile (/= 0) $ looseMarshall obj
  (signature, err) <- runPipe "." gpgProgram args payload
  unless (L.null err) $ putStrLn $ "Warning: " ++ pack (L8.unpack err)
  return signature



---------------------------------------------------
-- Helper functions, that run external processes --
---------------------------------------------------

-- | Run an external process.
run
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> IO ()
run dir cmd args = do
  putStrLn $ concatMap pack ["Running in ", dir, ": ", showCommandForUser cmd args]
  withCheckedProcessCleanup
    (proc cmd args)
    { cwd = Just dir
    }
    (\Inherited Inherited Inherited -> return ())


-- | Run an external process, pipes @stdin@ to it and returns @(stdout,
-- stderr)@. Throws an error if process exited abnormally.
runPipe
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> LByteString -- ^ @stdin@
  -> IO (LByteString, LByteString)
runPipe dir cmd args input = do
  putStrLn $ concatMap pack ["Running in ", dir, ": ", showCommandForUser cmd args]
  (exitCode, out, err) <-
    sourceProcessWithStreams
      (proc cmd args)
      { cwd = Just dir
      }
      (sourceLazy input)
      sinkLazyBuilder
      sinkLazyBuilder
  case exitCode of
    ExitSuccess -> return (out, err)
    code ->
      error $
      "Stackage.Package.Location.runPipe: " ++
      showCommandForUser cmd args ++ " produced an error: " ++ show code
