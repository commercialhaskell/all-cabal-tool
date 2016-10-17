{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stackage.Package.Locations
  ( hackageBaseUrl
  , hackageDeprecatedUrl
  , mirrorFPComplete
  , GitUser(..)
  , GitRepository(..)
  , Repositories(..)
  , GitFile
  , getGitFileName
  , readGitFile
  , ensureGitRepository
  , repoReadFile
  , repoReadFile'
  , repoWriteFile
  , repoWriteFile_
  , repoCommit
  , repoTag
    -- * Helper functons, that run external processes
  , run
  , runPipe
  ) where

--import System.FilePath (dropFileName, (</>))
--import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Control.Monad (unless, void)
import Data.Conduit.Process (withCheckedProcessCleanup, sourceProcessWithStreams, Inherited(Inherited))
import ClassyPrelude.Conduit (sourceLazy, sinkLazyBuilder)
import Data.Git
import Data.Git.Repository
import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import Data.String
import Data.Hourglass (timeFromElapsed)
import Time.System (timeCurrent)
import System.Directory
import System.FilePath
import System.Exit
import System.Process -- (proc, showCommandForUser)
import qualified Data.ByteString.UTF8 as U8
import qualified Filesystem.Path as P
import qualified Filesystem.Path.Rules as P


hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"


hackageDeprecatedUrl :: String
hackageDeprecatedUrl = hackageBaseUrl ++ "/packages/deprecated.json"


mirrorFPComplete :: String
mirrorFPComplete = "https://s3.amazonaws.com/hackage.fpcomplete.com"


data GitUser = GitUser
  { userName :: String
  , userEmail :: String
  , userGPG :: String
  } deriving (Show)



data GitRepository = GitRepository
  { repoAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , repoBranch :: String
    -- ^ Branch that updates should be committed to.
  , repoUser :: GitUser
    -- ^ User information to be used for the commits.
  , repoTagName :: Maybe String
    -- ^ Create a tag after an update.
  , repoLocalPath :: FilePath
    -- ^ Filepath to the root of the repository.
  , repoWorkTree :: WorkTree
    -- ^ Tree of all modifications to the repository.
  }

data Repositories = Repositories
  { allCabalFiles :: GitRepository
  , allCabalHashes :: GitRepository
  , allCabalMetadata :: GitRepository
  }


data GitFile = GitFile
  { _gitRepoPath :: P.FilePath
  , _gitFileRef :: Ref
  , _gitFileName :: FilePath
  }


-- | Get file name relative to it's git repository.
getGitFileName :: GitFile -> FilePath
getGitFileName = _gitFileName



-- | Read the file from repository by the reference.
readGitFile :: GitFile -> IO L.ByteString
readGitFile GitFile {..} =
  withRepo _gitRepoPath $
  \git -> do
    mobj <- getObject git _gitFileRef True
    let funcName = "Stackage.Package.Locations.readGitFile: "
    case mobj of
      Just (ObjBlob (Blob blob)) -> return blob
      Just _ ->
        error $ funcName ++ "Attempt to read a reference that is not a file"
      _ -> error $ funcName ++ "Reference to the file does not exist."



-- | Read a file from a repository. A tree that is pointed to by the
-- `repoBranch` is used instead of HEAD.
repoReadFile :: GitRepository -> FilePath -> IO (Maybe L.ByteString)
repoReadFile repo@GitRepository {..} fp = do
  withBranch repo $
    \git branchRef -> do
      mblobRef <- resolvePath git branchRef (toEntPath fp)
      case mblobRef of
        Nothing -> return Nothing
        Just blobRef -> do
          mobj <- getObject git blobRef True
          case mobj of
            Just (ObjBlob (Blob blob)) -> return $ Just blob
            _ -> return Nothing


-- | Same as `readRepoFile`, but will raise an error if file cannot be found.
repoReadFile' :: GitRepository -> FilePath -> IO L.ByteString
repoReadFile' repo@GitRepository {..} fp = do
  mfile <- repoReadFile repo fp
  case mfile of
    Just file -> return file
    Nothing ->
      error $
      "Could not find file: " ++
      fp ++ " in repository: " ++ repoLocalPath ++ " under branch: " ++ repoBranch


-- | Writes a file into the repository as a loose object. Use `repoCommit` to
-- finilize the changes. Returns `GitFile` that can be used to read the contents
-- of the file back at any time with the help of `readGitFile`.
repoWriteFile :: GitRepository -> FilePath -> L.ByteString -> IO GitFile
repoWriteFile GitRepository{..} fp blob = do
  let repoPath = getRepoPath repoLocalPath
  withRepo repoPath $
    \git -> do
      newRef <- setObject git (ObjBlob (Blob blob))
      workTreeSet git repoWorkTree (toEntPath fp) (EntFile, newRef)
      return (GitFile repoPath newRef fp)


-- | Same as `repoWriteFile` except it discards the result.
repoWriteFile_ :: GitRepository -> FilePath -> L.ByteString -> IO ()
repoWriteFile_ repo fp = void . repoWriteFile repo fp


-- | Flushes the work tree and creates a signed commit. Attached branch will
-- point to the commit after it is created. Returned is the repository with a
-- new work tree, unless there is nothing to commit, in which case it's a noop.
repoCommit :: GitRepository -- ^ Repository
           -> S8.ByteString -- ^ Action that creates a commit message.
           -> IO (Maybe GitRepository)
repoCommit repo@GitRepository {..} commitMessage = do
  withBranch repo $
    \git branchRef -> do
      oldRootRef <-
        maybe
          (error $
           "Cannot resolve root for " ++ repoBranch ++ " for repo: " ++ repoLocalPath)
          id <$>
        resolvePath git branchRef []
      newRootRef <- workTreeFlush git repoWorkTree
      print $ "Branch ref: " ++ show branchRef
      print $ "Old root ref: " ++ show oldRootRef
      print $ "New root ref: " ++ show newRootRef
      if oldRootRef == newRootRef
        then do
          putStrLn $ repoLocalPath ++ ": Nothing to commit"
          return Nothing
        else do
          person <- getPerson repoUser
          let commit =
                Commit
                { commitTreeish = newRootRef
                , commitParents = [branchRef]
                , commitAuthor = person
                , commitCommitter = person
                , commitEncoding = Nothing
                , commitExtras = []
                , commitMessage = commitMessage
                }
          signedCommit <- signCommit repo commit
          putStrLn $ "Will commit: " ++ show signedCommit
          commitRef <- setObject git (ObjCommit signedCommit)
          putStrLn $ "Created commit: " ++ show commitRef
          branchWrite git (RefName repoBranch) commitRef
          -- reset the tree
          workTree <- workTreeFrom newRootRef
          return $
            Just
            repo
            { repoWorkTree = workTree
            }


-- | Creates a signed tag of the commit, that a branch is pointing to.
repoTag :: GitRepository -> S8.ByteString -> IO ()
repoTag repo@GitRepository {repoTagName = Just tagStr, ..} tagMessage = do
  withBranch repo $
    \git branchRef -> do
      person <- getPerson repoUser
      let tag = Tag { tagRef = branchRef
                    , tagObjectType = TypeCommit
                    , tagBlob = S8.pack tagStr
                    , tagName = person
                    , tagS = tagMessage }
      signedTag <- signTag repo tag
      ref <- setObject git (ObjTag signedTag)
      putStrLn $ "Created tag: " ++ show ref
      tagWrite git (RefName tagStr) ref
      putStrLn $ "Wrote tag: " ++ show ref
repoTag _ _ = return ()      

----------------------------------------
-- Git manipulation helper functions. --
----------------------------------------


getRepoPath :: FilePath -> P.FilePath
getRepoPath repoLocalPath =
  P.decodeString P.posix repoLocalPath P.</> ".git"


toEntPath :: FilePath -> EntPath
toEntPath = map (entName . U8.fromString) . splitDirectories


getPerson :: GitUser -> IO Person
getPerson GitUser {..} = do
  now <- timeCurrent
  return $
    Person
    { personName = S8.pack userName
    , personEmail = S8.pack userEmail
    , personTime = timeFromElapsed now
    }


withBranch :: GitRepository -> (Git -> Ref -> IO a) -> IO a
withBranch GitRepository {..} action =
  withRepo (getRepoPath repoLocalPath) $
  \git -> do
    branchRef <-
      maybe
        (error $ "Cannot resolve " ++ repoBranch ++ " for repository: " ++ repoLocalPath)
        id <$>
      resolveRevision git (fromString repoBranch)
    action git branchRef


signCommit :: GitRepository -> Commit -> IO Commit
signCommit repo commit = do
  signature <- signObject repo (ObjCommit commit)
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


signTag :: GitRepository -> Tag -> IO Tag
signTag repo tag = do
  signature <- signObject repo (ObjTag tag)
  return
    tag
    { tagS = S8.intercalate "\n" [tagS tag, L.toStrict signature]
    }


signObject :: GitRepository -> Object -> IO L.ByteString
signObject GitRepository {..} obj = do
  gpgProgram <-
    withRepo (getRepoPath repoLocalPath) $
    \git -> maybe "gpg" id <$> configGet git "gpg" "program"
  let args = ["-bsau", userGPG repoUser]
  let payload = L.tail $ L.dropWhile (/= 0) $ looseMarshall obj
  (signature, err) <- runPipe "." gpgProgram args payload
  unless (L.null err) $ putStrLn $ "Warning: " ++ L8.unpack err
  return signature




-- | Clones the repo if it doesn't exists locally yet, otherwise fetches and
-- creates a local branch.
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
      run repoLocalPath "git" ["remote", "set-url", "origin", repoAddress]
      run repoLocalPath "git" ["fetch"]
      run repoLocalPath "git" ["branch", "-D", repoBranch]
      run
        repoLocalPath
        "git"
        ["checkout", "-B", repoBranch, "origin/" ++ repoBranch]
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
  -- initialize the work tree
  withBranch repo $
    \git branchRef -> do
      rootTreeRef <-
        maybe
          (error $
           "Cannot resolve root for " ++ repoBranch ++ " for repo: " ++ repoLocalPath)
          id <$>
        resolvePath git branchRef []
      workTree <- workTreeFrom rootTreeRef
      return
        repo
        { repoWorkTree = workTree
        }
  where
    repo =
      GitRepository
      { repoAddress = repoAddress
      , repoBranch = repoBranch
      , repoUser = gitUser
      , repoTagName = Nothing
      , repoLocalPath = repoLocalPath
      , repoWorkTree = error "Work Tree has not been initialized yet."
      }
    repoLocalPath = repoBasePath </> repoName
    repoAddress =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]


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
  putStrLn $ concat ["Running in ", dir, ": ", showCommandForUser cmd args]
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
  -> L.ByteString -- ^ @stdin@
  -> IO (L.ByteString, L.ByteString)
runPipe dir cmd args input = do
  putStrLn $ concat ["Running in ", dir, ": ", showCommandForUser cmd args]
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
