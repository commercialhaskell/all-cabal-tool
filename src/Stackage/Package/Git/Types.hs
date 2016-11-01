{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Package.Git.Types where

import ClassyPrelude.Conduit
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as BU
import Data.Git.Ref
import Data.Hourglass (timeFromElapsed)
import qualified Data.Map as Map
import Foreign.Storable (peekByteOff)
import Time.System (timeCurrent)

import qualified Data.Git as G

-- | User that will be used for creating commits (both author and commiter) and
-- tags.
data GitUser = GitUser
  { userName :: ByteString
    -- ^ Full name
  , userEmail :: ByteString
    -- ^ Email
  , userGpgKey :: Maybe String
    -- ^ GPG public key.
  } deriving (Show)


-- | Information about the git repository.
data GitInfo = GitInfo
  { gitAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , gitBranchName :: String
    -- ^ Name of the branch that updates should be committed to. (Remote branch
    -- with the same name is expected to exist).
  , gitUser :: GitUser
    -- ^ User information to be used for the commits.
  , gitTagName :: Maybe ByteString
    -- ^ Create a tag of a commit.
  , gitLocalPath :: FilePath
    -- ^ Where in the file system this repository is located.
  , gitRootTree :: Maybe (WorkTree ShortRef ShortRef)
  }


data GitInstance = GitInstance
  { gitRepo :: G.Git
    -- ^ Git repository instance.
  , gitBranchRef :: MVar G.Ref
    -- ^ Reference of the commit, that current branch is pointing to.
  , gitWorkTree :: MVar (WorkTree ShortRef ShortRef, WorkTree () GitFile)
    -- ^ Current work tree written to the git store.
  }


-- | Everything that is necessary to operate on a repository.
data GitRepository = GitRepository
  { repoInstance :: GitInstance
  , repoInfo :: GitInfo
  }



-- | A file that is prepared for being written into git store.
data GitFile = GitFile
  { gitFileRef :: !G.Ref
    -- ^ SHA1 of the blob.
  , gitFileType :: !FileType
    -- ^ One of the few possible file types.
  , gitFileZipped :: !ByteString
    -- ^ A blob object: marshalled and compressed file.
  } deriving (Show)

type GitTree = G.Tree

type GitCommit = G.Commit

type GitTag = G.Tag

-- | This data type is crutial for keeping files properly sorted in the work
-- tree, in a way that is expected by git. In particular it expects directories
-- to have a trailing slash @/@, and it follows some properties:
--
-- @@@
-- λ> FileName "foo" == DirectoryName "foo/"
-- True
-- λ> DirectoryName "foo-0.1/" > DirectoryName "foo-0.1.1/"
-- True
-- λ> "foo-0.1" > "foo-0.1.1" -- <= different from above
-- False
-- @@@
--
data FileName
  = FileName !BS.ShortByteString
    -- ^ Just a file name without a path to it.
  | DirectoryName !BS.ShortByteString
    -- ^ Directory name with a trailing slash, without it trees wouldn't get
    -- sorted properly: @("0.1" > "0.1.1") /= ("0.1/" > "0.1.1/")@

instance Show FileName where
  show (FileName fName) = show fName
  show (DirectoryName dirName) = show dirName

type TreePath = [FileName]


-- | Convert file path to a tree path.
toTreePath :: FilePath -> TreePath
toTreePath path = foldr toFileName [] splitPath'
  where
    splitPath' = S8.split '/' $ S8.pack path
    toFileName fName []
      | null fName =
        error $ "Tree path should end with a file name, not a directory: " ++ path
      | otherwise = [FileName $ BS.toShort fName]
    toFileName fDir tp = DirectoryName (BS.toShort $ S8.snoc fDir '/') : tp


instance Eq FileName where
  (==) (FileName f1) (FileName f2) = f1 == f2
  (==) (DirectoryName d1) (DirectoryName d2) = d1 == d2
  (==) f1 f2 = fileNameToBS f1 == fileNameToBS f2

fileNameToBS :: FileName -> ByteString
fileNameToBS !(FileName f) = BS.fromShort f
fileNameToBS !(DirectoryName d) = S8.init (BS.fromShort d)


instance Ord FileName where
  compare (FileName f1) (FileName f2) = compare f1 f2
  compare f@(FileName fn) d@(DirectoryName dn)
    | f == d = EQ
    | otherwise = compare fn dn
  compare d@(DirectoryName dn) f@(FileName fn)
    | f == d = EQ
    | otherwise = compare dn fn
  compare (DirectoryName d1) (DirectoryName d2) = compare d1 d2


-- | A very tight representation of SHA1 hash.
data ShortRef =
  ShortRef !Word64
           !Word64
           !Word32
  deriving (Eq, Ord)


-- | Reduce memory usage by squeezing `Ref` into a much more memory efficient
-- representation `ShortRef`.
toShortRef :: MonadIO m => Ref -> m ShortRef
toShortRef !(toBinary -> bs)
    | length bs /= 20 = error "Stackage.Package.Git.Types.toShortRef: length is not 20"
    | otherwise = liftIO $ BU.unsafeUseAsCString bs $ \ptr -> ShortRef
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16

-- | Convert `ShortRef` back into a `Ref`, so it can be used with Hit package.
fromShortRef :: ShortRef -> Ref
fromShortRef !(ShortRef w64 w64' w32) =
  fromBinary $
  L.toStrict $
  B.toLazyByteString
    (BE.word64Host w64 <> BE.word64Host w64' <> BE.word32Host w32)


-- | All possible file types expected by git.
data FileType
  = ExecFile
    -- ^ Executable - @0o100755@
  | NonExecFile
    -- ^ Normal file - @0o100644@
  | NonExecGroupFile
    -- ^ Grandfathered group writeable mode - @0o100664@ - @git fsck@ allows
    -- this permissions:
    -- <https://github.com/git/git/blob/8354fa3d4ca50850760ceee9054e3e7a799a4d62/fsck.c#L583>
  | SymLink
    -- ^ Symbolic link - @0o120000@
  | GitLink
    -- ^ Git link, used for modules - @0o160000@
  deriving (Eq, Show)


-- | A recursive git tree that can either be used to keep current representation
-- of tree or simulate an in-memory work tree.
data WorkTree d f
  = File !f !FileType
  | Directory !d !(Map.Map FileName (WorkTree d f))
  deriving (Show)


-- | This instance on directories only checks equality of SHAs, which means all
-- subtrees are also equal, hence avoiding descend into all subtrees.
instance Eq (WorkTree ShortRef ShortRef) where
  (File ref1 t1) == (File ref2 t2) = ref1 == ref2 && t1 == t2
  (Directory ref1 _) == (Directory ref2 _) = ref1 == ref2
  _ == _ = False
  

-- | Create a `Person` from `GitUser` by supplying current time.
getPerson :: GitUser -> IO G.Person
getPerson GitUser {..} = do
  now <- timeCurrent
  return $
    G.Person
    { G.personName = userName
    , G.personEmail = userEmail
    , G.personTime = timeFromElapsed now
    }


getTreeRef :: WorkTree ShortRef ShortRef -> ShortRef
getTreeRef (Directory ref _) = ref
getTreeRef (File ref _) = ref


getTreeMode :: WorkTree d f -> G.ModePerm
getTreeMode Directory {}     = G.ModePerm 0o040000
getTreeMode (File _ GitLink) = G.ModePerm 0o160000
getTreeMode (File _ SymLink) = G.ModePerm 0o120000
getTreeMode (File _ ExecFile) = G.ModePerm 0o100755
getTreeMode (File _ NonExecFile) = G.ModePerm 0o100644
getTreeMode (File _ NonExecGroupFile) = G.ModePerm 0o100664
