{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Stackage.Package.Locations
  ( hackageBaseUrl
  , hackageDeprecatedUrl
  , mirrorFPComplete
  , GitUser(..)
  , GitInfo(..)
  , GitInstance
  , Repository(..)
  , withRepository
  , Repositories(..)
  , withRepositories
  , ensureRepository
  , repoReadFile
  , repoReadFile'
  , repoWriteFile
  , repoCommit
  , repoTag
   -- * Helper functons, that run external processes
  , run
  , runPipe
  ) where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Control.Monad (unless)
import Data.Conduit.Process
       (withCheckedProcessCleanup, sourceProcessWithStreams,
        Inherited(Inherited))
import ClassyPrelude.Conduit (sourceLazy, sinkLazyBuilder)
import Data.Git
import Data.Git.Ref (fromHex)
import Data.Git.Repository
import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import Data.String
import Data.Hourglass (timeFromElapsed)
import Time.System (timeCurrent)
import System.Directory
import System.FilePath
import System.Exit
import System.Process (proc, cwd, showCommandForUser)
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



data GitInfo = GitInfo
  { gitAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , gitBranchName :: String
    -- ^ Branch that updates should be committed to.
  , gitUser :: GitUser
    -- ^ User information to be used for the commits.
  , gitTagName :: Maybe String
    -- ^ Create a tag after an update.
  , gitLocalPath :: FilePath
  }

data GitInstance = GitInstance
  { gitRepo :: Git
    -- ^ Git repository instance.
  , gitBranchRef :: Ref
    -- ^ Reference of the commit, that current branch is pointing to.
  , gitWorkTree :: WorkTree
    -- ^ Tree of all modifications to the repository.
  }


data Repository = Repository
  { repoInstance :: GitInstance
  , repoInfo :: GitInfo
  }


data Repositories = Repositories
  { allCabalFiles :: Repository
  , allCabalHashes :: Repository
  , allCabalMetadata :: Repository
  }


withRepositories
  :: (GitInfo, GitInfo, GitInfo) -> (Repositories -> IO a) -> IO a
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


-- | Read a file from a repository. A tree that is pointed to by the
-- `repoBranch` is used instead of HEAD.
repoReadFile :: Repository -> FilePath -> IO (Maybe L.ByteString)
repoReadFile Repository {repoInstance = GitInstance {..}} fp = do
  mblobRef <- resolvePath gitRepo gitBranchRef (toEntPath fp)
  case mblobRef of
    Nothing -> return Nothing
    Just blobRef -> do
      mobj <- getObject gitRepo blobRef True
      case mobj of
        Just (ObjBlob (Blob blob)) -> return $ Just blob
        _ -> return Nothing

-- | Same as `readRepoFile`, but will raise an error if file cannot be found.
repoReadFile' :: Repository -> FilePath -> IO L.ByteString
repoReadFile' repo@Repository {repoInfo = GitInfo {..}} fp = do
  mfile <- repoReadFile repo fp
  case mfile of
    Just file -> return file
    Nothing ->
      error $
      "Could not find file: " ++
      fp ++ " in repository: " ++ gitLocalPath ++ " under branch: " ++ gitBranchName


-- | Writes a file into the repository as a loose object. Use `repoCommit` to
-- finilize the changes. Returns `GitFile` that can be used to read the contents
-- of the file back at any time with the help of `readGitFile`.
repoWriteFile :: Repository -> FilePath -> L.ByteString -> IO ()
repoWriteFile Repository {repoInstance = GitInstance {..}
                         ,repoInfo = GitInfo {..}} fp blob = do
  newRef <- setObject gitRepo (ObjBlob (Blob blob))
  {-(newRef, _) <-
    runPipe
      gitLocalPath
      "git"
      ["hash-object", "-w", "-t", "blob", "--path=" ++ fp, "--stdin"]
      blob -}
  workTreeSet gitRepo gitWorkTree (toEntPath fp) (EntFile, newRef)


-- | Flushes the work tree and creates a signed commit. Attached branch will
-- point to the commit after it is created.
repoCommit :: Repository -- ^ Repository
           -> S8.ByteString -- ^ Action that creates a commit message.
           -> IO ()
repoCommit Repository {repoInstance = GitInstance {..}
                      ,repoInfo = GitInfo {..}} commitMessage = do
  oldRootTreeRef <-
    maybe
      (error $ "Cannot resolve root for " ++ gitBranchName ++ " for repo: " ++ gitLocalPath)
      id <$>
    resolvePath gitRepo gitBranchRef []
  newRootTreeRef <- workTreeFlush gitRepo gitWorkTree
  print $ "Branch ref: " ++ show gitBranchRef
  print $ "Old root ref: " ++ show oldRootTreeRef
  print $ "New root ref: " ++ show newRootTreeRef
  if oldRootTreeRef == newRootTreeRef
    then do
      putStrLn $ gitLocalPath ++ ": Nothing to commit"
    else do
      person <- getPerson gitUser
      let commit =
            Commit
            { commitTreeish = newRootTreeRef
            , commitParents = [gitBranchRef]
            , commitAuthor = person
            , commitCommitter = person
            , commitEncoding = Nothing
            , commitExtras = []
            , commitMessage = commitMessage
            }
      signedCommit <- signCommit gitRepo (userGPG gitUser) commit
      putStrLn $ "Will commit: " ++ show signedCommit
      commitRef <- setObject gitRepo (ObjCommit signedCommit)
      putStrLn $ "Created commit: " ++ show commitRef
      branchWrite gitRepo (RefName gitBranchName) commitRef


-- | Creates a signed tag of the commit, that a branch is pointing to.
repoTag :: Repository -> S8.ByteString -> IO ()
repoTag Repository {repoInstance = GitInstance {..}
                   ,repoInfo = GitInfo {gitTagName = Just tagStr
                                       ,..}} tagMessage = do
  person <- getPerson gitUser
  let tag =
        Tag
        { tagRef = gitBranchRef
        , tagObjectType = TypeCommit
        , tagBlob = S8.pack tagStr
        , tagName = person
        , tagS = tagMessage
        }
  signedTag <- signTag gitRepo (userGPG gitUser) tag
  ref <- setObject gitRepo (ObjTag signedTag)
  putStrLn $ "Created tag: " ++ show ref
  tagWrite gitRepo (RefName tagStr) ref
  putStrLn $ "Wrote tag: " ++ show ref
repoTag _ _ = return ()      

----------------------------------------
-- Git manipulation helper functions. --
----------------------------------------


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


withRepository :: GitInfo -> (Repository -> IO a) -> IO a
withRepository info@GitInfo {..} action =
  withRepo (P.decodeString P.posix gitLocalPath P.</> ".git") $
  \git -> do
    branchRef <-
      maybe
        (error $ "Cannot resolve " ++ gitBranchName ++ " for repository: " ++ gitLocalPath)
        id <$>
      resolveRevision git (fromString gitBranchName)
    rootTreeRef <-
      maybe
        (error $
         "Cannot resolve root tree for " ++
         gitBranchName ++ " for repository: " ++ gitLocalPath)
        id <$>
      resolvePath git branchRef []
    workTree <- workTreeFrom rootTreeRef
    action
      Repository
      { repoInstance =
        GitInstance
        { gitRepo = git
        , gitBranchRef = branchRef
        , gitWorkTree = workTree
        }
      , repoInfo = info
      }
    

signCommit :: Git -> String -> Commit -> IO Commit
signCommit gitRepo key commit = do
  signature <- signObject gitRepo key (ObjCommit commit)
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


signTag :: Git -> String -> Tag -> IO Tag
signTag gitRepo key tag = do
  signature <- signObject gitRepo key (ObjTag tag)
  return
    tag
    { tagS = S8.intercalate "\n" [tagS tag, L.toStrict signature]
    }


signObject :: Git -> String -> Object -> IO L.ByteString
signObject gitRepo key obj = do
  gpgProgram <- maybe "gpg" id <$> configGet gitRepo "gpg" "program"
  let args = ["-bsau", key]
  let payload = L.tail $ L.dropWhile (/= 0) $ looseMarshall obj
  (signature, err) <- runPipe "." gpgProgram args payload
  unless (L.null err) $ putStrLn $ "Warning: " ++ L8.unpack err
  return signature




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
      run repoLocalPath "git" ["branch", "-D", repoBranchName]
      run
        repoLocalPath
        "git"
        ["checkout", "-B", repoBranchName, "origin/" ++ repoBranchName]
    else run
           "."
           "git"
           [ "clone"
           , "--depth=1"
           , repoAddress
           , repoLocalPath
           , "--branch"
           , repoBranchName
           ]
  run repoLocalPath "git" ["config", "user.name", userName gitUser]
  run repoLocalPath "git" ["config", "user.email", userEmail gitUser]
  run repoLocalPath "git" ["config", "core.autocrlf", "input"]
  -- initialize the work tree
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
  --putStrLn $ concat ["Running in ", dir, ": ", showCommandForUser cmd args]
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
  --putStrLn $ concat ["Running in ", dir, ": ", showCommandForUser cmd args]
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
