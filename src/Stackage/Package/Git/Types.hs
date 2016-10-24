{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Package.Git.Types where

import ClassyPrelude.Conduit
import qualified Data.ByteString.Char8 as S8
import Data.Bits
import qualified Data.Map as Map
import Data.Word (Word32)
import Data.Hourglass (timeFromElapsed)
import Time.System (timeCurrent)

import qualified Data.Git as G


data GitUser = GitUser
  { userName :: ByteString
  , userEmail :: ByteString
  , userGpgKey :: Maybe String
  } deriving (Show)



data GitInfo = GitInfo
  { gitAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , gitBranchName :: String
    -- ^ Branch that updates should be committed to.
  , gitUser :: GitUser
    -- ^ User information to be used for the commits.
  , gitTagName :: Maybe ByteString
    -- ^ Create a tag after an update.
  , gitLocalPath :: FilePath
  }



data GitInstance = GitInstance
  { gitRepo :: G.Git
    -- ^ Git repository instance.
  , gitBranchRef :: MVar G.Ref
    -- ^ Reference of the commit, that current branch is pointing to.
  , gitRootTree :: MVar (WorkTree G.Ref G.Ref)
  , gitWorkTree :: MVar (WorkTree () GitFile)
    -- ^ Current work tree written to the git store.
  }


data GitRepository = GitRepository
  { repoInstance :: GitInstance
  , repoInfo :: GitInfo
  }




data GitFile = GitFile
  { gitFileRef :: !G.Ref
  , gitFileType :: !FileType
  , gitFileZipped :: !ByteString
  } deriving (Show)

type GitTree = G.Tree

type GitCommit = G.Commit

type GitTag = G.Tag


data FileName
  = FileName !ByteString
  | DirectoryName !ByteString
  deriving (Show)


type TreePath = [FileName]

toTreePath :: FilePath -> TreePath
toTreePath path = foldr toFileName [] splitPath'
  where
    splitPath' = S8.split '/' $ S8.pack path
    toFileName fName []
      | null fName =
        error $ "Tree path should end with a file name, not a directory: " ++ path
      | otherwise = [FileName fName]
    toFileName fDir tp = DirectoryName (S8.snoc fDir '/') : tp




instance Eq FileName where
  (==) (FileName f1) (FileName f2) = f1 == f2
  (==) (FileName f) (DirectoryName d) = f == S8.init d
  (==) (DirectoryName d) (FileName f) = S8.init d == f
  (==) (DirectoryName d1) (DirectoryName d2) = d1 == d2


instance Ord FileName where
  compare (FileName f1) (FileName f2) = compare f1 f2
  compare f@(FileName fn) d@(DirectoryName dn)
    | f == d = EQ
    | otherwise = compare fn dn
  compare d@(DirectoryName dn) f@(FileName fn)
    | f == d = EQ
    | otherwise = compare dn fn
  compare (DirectoryName d1) (DirectoryName d2) = compare d1 d2


data UnixPerm
  = ExecFile
    -- ^ Executable - @0o755@
  | NonExecFile
    -- ^ Normal file - @0o644@
  | NonExecGroupFile
    -- ^ Grandfathered mode - @0o664@ - @git fsck@ allows this permissions:
    -- <https://github.com/git/git/blob/8354fa3d4ca50850760ceee9054e3e7a799a4d62/fsck.c#L583>
  deriving (Eq, Show)


data FileType
  = RegularFile !UnixPerm
  | SymLink
  | GitLink
  deriving (Eq, Show)
  

data WorkTree d f
  = File !f !FileType
  | Directory !d !(Map.Map FileName (WorkTree d f))
  deriving (Show)



getPerson :: GitUser -> IO G.Person
getPerson GitUser {..} = do
  now <- timeCurrent
  return $
    G.Person
    { G.personName = userName
    , G.personEmail = userEmail
    , G.personTime = timeFromElapsed now
    }




getTreeRef :: WorkTree G.Ref G.Ref -> G.Ref
getTreeRef (Directory ref _) = ref
getTreeRef (File ref _) = ref


getTreeMode :: WorkTree d f -> G.ModePerm
getTreeMode Directory {}     = G.ModePerm 0o040000
getTreeMode (File _ GitLink) = G.ModePerm 0o160000
getTreeMode (File _ SymLink) = G.ModePerm 0o120000
getTreeMode (File _ (RegularFile perm)) = G.ModePerm (0o100000 .|. fromUnixPerm perm)


fromUnixPerm :: UnixPerm -> Word32
fromUnixPerm ExecFile = 0o755
fromUnixPerm NonExecFile = 0o644
fromUnixPerm NonExecGroupFile = 0o664


toUnixPerm :: Word32 -> UnixPerm
toUnixPerm perm = case (perm .&. 0o777) of
  0o755 -> ExecFile
  0o644 -> NonExecFile
  0o664 -> NonExecGroupFile
  p -> error $ "Unrecognized file permission: " ++ show p
