{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Git.Repository
  ( ensureRepository
  , withRepository
  , repoReadFile
  , repoReadFile'
  , repoWriteFile
  , repoWriteGitFile
  , repoCreateCommit
  , repoCreateTag
   -- * Helper functions, that run external processes
  , run
  , runPipe
  , runPipeWithConduit
  ) where

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


-- | Initializes the work tree necessary for operating on a repository. Grabs
-- some files from the git store and does appropriate cleanup when supplied action
-- has terminated.
withRepository
  :: GitInfo -- ^ All information required for initializing the
     -- repository. Use `ensureRepository` to create it.
  -> (GitRepository -> IO a) -- ^ Action to be performed on the
     -- repository.
  -> IO (GitInfo, a) -- ^ Returned info will contain current root tree.
withRepository info@GitInfo {..} action = do
  properRepo <- isRepo (P.decodeString gitLocalPath)
  unless properRepo $ error $ "There is no git repository in :" ++ gitLocalPath
  withRepo (P.decodeString gitLocalPath) $
    \git -> do
      branchRef <-
        maybe
          (error $
           "Cannot resolve " ++ gitBranchName ++ " for repository: " ++ gitLocalPath)
          return =<<
        resolveRevision git (fromString gitBranchName)
      headSet git (Right (RefName gitBranchName))
      branchRefMVar <- newMVar branchRef
      rootTree <-
        case gitRootTree of
          Just rootTree -> return rootTree
          Nothing -> do
            rootTreeRef <-
              maybe
                (error $
                 "Cannot resolve root tree for '" ++
                 gitBranchName ++ "' branch inside repository: " ++ gitLocalPath)
                id <$>
              resolvePath git branchRef []
            readWorkTree git rootTreeRef
      workTreeMVar <- newMVar (rootTree, emptyWorkTree)
      res <-
        action
          GitRepository
          { repoInstance =
            GitInstance
            { gitRepo = git
            , gitBranchRef = branchRefMVar
            , gitWorkTree = workTreeMVar
            }
          , repoInfo = info
          }
      newInfo <-
        withMVar workTreeMVar $
        \(newRootTree, _) ->
           return $
           info
           { gitRootTree = Just newRootTree
           }
      return (newInfo, res)



-- | Reads a file from the repository. In case an updated verion the file exists
-- in a work tree it will be used instead of one in the repo. Returns `Nothing`
-- if the file cannot be found under the supplied `FilePath`.
repoReadFile :: GitRepository -> FilePath -> IO (Maybe LByteString)
repoReadFile GitRepository {repoInstance = GitInstance {..}} fp = do
  withMVar gitWorkTree $
    \(rootTree, workTree) -> do
      let treePath = toTreePath fp
      case lookupFile workTree treePath of
        Just f -> do
          let (_, blob) =
                looseUnmarshallZippedRaw $
                Zipped $ L.fromStrict $ gitFileZipped f
          return $ Just blob
        Nothing -> do
          case lookupFile rootTree treePath of
            Just sRef -> do
              mobj <- getObject gitRepo (fromShortRef sRef) True
              case mobj of
                Just (G.ObjBlob (G.Blob blob)) -> return $ Just blob
                _ -> return Nothing
            Nothing -> return Nothing



-- | Same as `readRepoFile`, but will throw an error if the file cannot be found.
repoReadFile' :: GitRepository -> FilePath -> IO LByteString
repoReadFile' repo@GitRepository {repoInfo = GitInfo {..}} fp = do
  mfile <- repoReadFile repo fp
  case mfile of
    Just file -> return file
    Nothing ->
      error $
      "Could not find file: " ++
      fp ++ " in repository: " ++ gitLocalPath ++ " under branch: " ++ gitBranchName


-- | Places a file into current in-memory work tree. Use `flushWorkTree` in
-- order to persist changes to disk.
repoWriteFile :: GitRepository -> FilePath -> LByteString -> IO ()
repoWriteFile repo fp f =
  makeGitFile f (fromIntegral $ length f) >>= repoWriteGitFile repo fp



-- | Same as `repoWriteFile` except it is faster, since it operates on a
-- `GitFile`, which is a version of the file that is already prepared for
-- writing as an object into the git repository.
repoWriteGitFile :: GitRepository -> FilePath -> GitFile -> IO ()
repoWriteGitFile GitRepository {repoInstance = GitInstance {..}} fp f = do
  let treePath = toTreePath fp
  modifyMVar_ gitWorkTree $
    \(rootTree, workTree) -> do
      newWorkTree <-
        case lookupFile rootTree treePath of
          Just sRef -> do
            sRef' <- toShortRef $ gitFileRef f
            return $!
              if sRef == sRef'
                then removeGitFile workTree treePath
                else insertGitFile workTree treePath f
          Nothing -> do
            return $! insertGitFile workTree treePath f
      return (rootTree, newWorkTree)


-- | Writes the changes made to the work tree onto the disk and creates a signed
-- commit using `userGpgKey` and commit message argument. Moves current branch
-- pointer to the newly created commit. `Ref`erence for the commit is returned
-- unless no real changes to the work tree was detected.
repoCreateCommit :: GitRepository -- ^ Current repository.
                 -> ByteString -- ^ Commit message.
                 -> IO (Maybe Ref)
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
        -- adjust for improper marshalling by hit package:
        let commit' = commit { commitMessage = S8.append (commitMessage commit) "\n" }
        newCommit <- case userGpgKey gitUser of
          Just key -> signCommit gitRepo key commit'
          Nothing -> return commit'
        commitRef <- repoWriteObject repo (Commit newCommit)
        branchWrite gitRepo (RefName gitBranchName) commitRef
        return (commitRef, Just commitRef)


-- | Creates a tag, that is optionally signed with `userGpgKey`. Tag will not be
-- created if `gitTagName` is `Nothing`, in which case `Nothing` is also
-- returned.
repoCreateTag :: GitRepository -- ^ Current repository
              -> Ref -- ^ Commit reference.
              -> ByteString -- ^ Tag message.
              -> IO (Maybe Ref)
repoCreateTag repo@GitRepository {repoInstance = GitInstance {..}
                                 ,repoInfo = GitInfo {gitTagName = Just tagName
                                                     ,..}} commitRef tagMessage = do
  tag <- makeGitTag commitRef gitUser tagName tagMessage
  -- adjust for improper marshalling by hit package:
  let tag' = tag { tagS = S8.append (tagS tag) "\n" }
  newTag <-
    case userGpgKey gitUser of
      Just key -> signTag gitRepo key tag'
      Nothing -> return tag'
  ref <- repoWriteObject repo (Tag newTag)
  writeFile (gitLocalPath </> "refs" </> "tags" </> S8.unpack (G.tagBlob newTag)) (S8.pack (show ref))
  return $ Just ref
repoCreateTag _ _ _ = return Nothing


-- | Clones the repo if it doesn't exists locally yet, otherwise fetches and
-- creates a local branch.
ensureRepository
  :: String -- ^ Git provider (e.g. "github.com")
  -> String -- ^ Repository account (e.g. "commercialhaskell")
  -> GitUser -- ^ User information to be used for commits and tags.
  -> String -- ^ Repository name (e.g. "all-cabal-files")
  -> String -- ^ Repository branch, make sure there is a remote branch with that
            -- name. (e.g. "master")
  -> FilePath -- ^ Location in the file system where repository should be cloned
     -- to.
  -> IO GitInfo
ensureRepository repoHost repoAccount gitUser repoName repoBranchName repoBasePath = do
  exists <- doesDirectoryExist repoLocalPath
  if exists
    then do
      run repoLocalPath "git" ["remote", "set-url", "origin", repoAddress]
      run
        repoLocalPath
        "git"
        ["fetch", "origin", repoBranchName <> ":" <> repoBranchName]
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
  return
    GitInfo
    { gitAddress = repoAddress
    , gitBranchName = repoBranchName
    , gitUser = gitUser
    , gitTagName = Nothing
    , gitLocalPath = repoLocalPath
    , gitRootTree = Nothing
    }
  where
    repoLocalPath = repoBasePath </> repoName
    repoAddress =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]


-- | Signs a commit.
signCommit :: Git -> String -> G.Commit -> IO G.Commit
signCommit gitRepo key commit = do
  signature <- signObject gitRepo key (G.ObjCommit commit)
  -- Workaround for: https://github.com/vincenthz/hit/issues/35
  -- Otherwise should be:
  -- let signatureKey = "gpgsig"
  --     signature = L.toStrict out
  let (sigBegin, sigRest) = L8.break (== '\n') signature
  when (sigBegin /= "-----BEGIN PGP SIGNATURE-----" || L.null sigRest) $
    error $
    "Stackage.Package.Git.Repository.signCommit: Malformed signature: \n" ++
    L8.unpack signature
  let signatureKey = L.toStrict $ L.append "gpgsig " sigBegin
      signature' = L.toStrict $ L.drop 1 sigRest
  return
    commit
    { commitExtras =
      commitExtras commit ++ [CommitExtra signatureKey signature']
    }

-- | Signs a tag
signTag :: Git -> String -> G.Tag -> IO G.Tag
signTag gitRepo key tag = do
  signature <- signObject gitRepo key (G.ObjTag tag)
  return
    tag
    { tagS = S8.append (tagS tag) (L.toStrict signature)
    }


-- | Signs an object (really useful only for either a commit or a tag) using a
-- program specified in git configuration under 'gpg.program'.
signObject :: Git -> String -> G.Object -> IO L.ByteString
signObject gitRepo key obj = do
  gpgProgram <- maybe "gpg" id <$> configGet gitRepo "gpg" "program"
  let args = ["-bsau", key]
  let (header, payload0) = L.break (== 0) $ looseMarshall obj
      objType = G.objectTypeMarshall (G.objectToType obj)
  when
    (L8.pack (objType ++ " " ++ show (L.length payload0 - 1)) /= header ||
     L.null payload0) $
    error $
    "Stackage.Package.Git.Repository.signObject: Marshalled object " ++
    objType ++ " is malformed."
  (signature, err) <- runPipe "." gpgProgram args $ L.drop 1 payload0
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


-- | Run an external process, pipe @stdin@ to it and return @(stdout,
-- stderr)@. Throws an error if process exited abnormally.
runPipe
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> LByteString -- ^ @stdin@
  -> IO (LByteString, LByteString)
runPipe dir cmd args input =
  runPipeWithConduit dir cmd args (sourceLazy input) sinkLazyBuilder sinkLazyBuilder


-- | Run an external process, pipe @stdin@ to it and sink @stdout@ and
-- @stderr@. Throws an error if process exited abnormally.
runPipeWithConduit
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> Producer IO ByteString -- ^ @stdin@ producer
  -> Consumer ByteString IO a -- ^ @stdout@ consumer
  -> Consumer ByteString IO b -- ^ @stderr@ consumer
  -> IO (a, b)
runPipeWithConduit dir cmd args inputSrc stdoutSink stderrSink = do
  putStrLn $
    concatMap pack ["Running in ", dir, ": ", showCommandForUser cmd args]
  (exitCode, out, err) <-
    sourceProcessWithStreams
      (proc cmd args)
      { cwd = Just dir
      }
      inputSrc
      stdoutSink
      stderrSink
  case exitCode of
    ExitSuccess -> return (out, err)
    code ->
      error $
      "Stackage.Package.Location.runPipeWithConduit: " ++
      showCommandForUser cmd args ++ " produced an error: " ++ show code
