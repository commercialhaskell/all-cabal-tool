{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Stackage.Package.Git.WorkTree where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Byteable
import Data.Git
       hiding (WorkTree, EntType(..), workTreeNew, workTreeFrom,
               workTreeDelete, workTreeSet, workTreeFlush)
import Data.Git.Storage
import Data.Git.Storage.Object
import qualified Data.Map as Map
import qualified Data.List as L
import Data.Word
import qualified Filesystem.Path.CurrentOS as P
import System.Directory
import System.FilePath

import Stackage.Package.Locations


data UnixPerm
  = ExecFile
    -- ^ Executable - @0o755@
  | NonExecFile
    -- ^ Normal file - @0o644@
  | NonExecGroupFile
    -- ^ Grandfathered mode - @0o664@ - @git fsck@ allows this permissions:
    -- <https://github.com/git/git/blob/8354fa3d4ca50850760ceee9054e3e7a799a4d62/fsck.c#L583>
  deriving Show


data FileName
  = FileName !S.ByteString
  | DirectoryName !S.ByteString
  deriving (Show)

instance Eq FileName where
  (==) (FileName f1) (FileName f2) = f1 == f2
  (==) (FileName f) (DirectoryName d) = f == S.init d
  (==) (DirectoryName d) (FileName f) = S.init d == f
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



data FileType
  = RegularFile !UnixPerm
  | SymLink
  | GitLink
  deriving (Show)
  

data Tree = WorkTree Ref Ref


data WorkTree d f
  = File f FileType
  | Directory d (Map.Map FileName (WorkTree d f))
  deriving (Show)

type WorkTreePath = [FileName]


emptyWorkTree :: WorkTree () f
emptyWorkTree = Directory () Map.empty


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



toWorkTreePath :: FilePath -> WorkTreePath
toWorkTreePath path = L.foldr toFileName [] splitPath
  where
    splitPath = S8.split '/' $ S8.pack path
    toFileName fName []
      | S.null fName =
        error $ "Tree path should end with a file name, not a directory: " ++ path
      | otherwise = [FileName fName]
    toFileName fDir tp = DirectoryName (S8.snoc fDir '/') : tp


insertCustomFile :: WorkTree () f -> FilePath -> FileType -> f -> WorkTree () f
insertCustomFile tree path fType f = insertFileRec tree (toWorkTreePath path)
  where
    insertFileRec _ [] = error "Cannot insert a file without a name"
    insertFileRec (Directory _ dirMap) [fName] =
      Directory () $ Map.insert fName (File f fType) dirMap
    insertFileRec (Directory _ dirMap) (dirName:pathTail) = Directory () newDirMap
      where
        newDirMap =
          case Map.lookup dirName dirMap of
            Just subTree@(Directory _ _) ->
              Map.insert dirName (insertFileRec subTree pathTail) dirMap
            _ ->
              Map.insert dirName (insertFileRec (Directory () Map.empty) pathTail) dirMap
    insertFileRec File {} treePath = insertFileRec (Directory () Map.empty) treePath


insertFile :: WorkTree () f -> FilePath -> f -> WorkTree () f
insertFile tree fp = insertCustomFile tree fp (RegularFile NonExecFile)


lookupFile :: WorkTree a f -> FilePath -> Maybe f
lookupFile tree path = getFile $ lookupRec tree (toWorkTreePath path)
  where
    getFile (Just (File f _)) = Just f
    getFile _ = Nothing                         
    lookupRec _ [] = error "Cannot lookup a file without a name."
    lookupRec (Directory _ dirMap) [fileName] = Map.lookup fileName dirMap
    lookupRec (Directory _ dirMap) (dirName:pathTail) =
      case Map.lookup dirName dirMap of
         Just subTree -> lookupRec subTree pathTail
         Nothing -> Nothing
    lookupRec File {} _ = Nothing


readWorkTree :: Git -> Ref -> IO (WorkTree Ref Ref)
readWorkTree repo commitRef = do
  mrootRef <- resolvePath repo commitRef []
  case mrootRef of
    Just ref -> readTreeRec ref
    Nothing -> error "Cannot resolve root tree of the commit."
  where
    readTreeFile (ModePerm mode, ent, ref)
      | mode == 0o120000 = return (FileName $ toBytes ent, File ref SymLink)
      | mode == 0o160000 = return (FileName $ toBytes ent, File ref GitLink)
      | (mode .&. 0o777000) == 0o100000 =
        return (FileName $ toBytes ent, File ref (RegularFile (toUnixPerm mode)))
      | mode == 0o040000 = do
          directory <- readTreeRec ref
          return (DirectoryName $ S8.snoc (toBytes ent) '/', directory)
      | otherwise = error $ "Unsupported file mode: " ++ show mode
    readTreeRec ref = do
      (Tree tree) <- getTree repo ref
      files <- mapM readTreeFile tree
      return $ Directory ref $ Map.fromAscList files


getTreeRef :: WorkTree Ref Ref -> Ref
getTreeRef (Directory ref _) = ref
getTreeRef (File ref _) = ref


getTreeMode :: WorkTree d f -> ModePerm
getTreeMode Directory {}     = ModePerm 0o040000
getTreeMode (File _ GitLink) = ModePerm 0o160000
getTreeMode (File _ SymLink) = ModePerm 0o120000
getTreeMode (File _ (RegularFile perm)) = ModePerm (0o100000 .|. fromUnixPerm perm)




persistGitFile :: Git -> GitFile -> IO Ref
persistGitFile repo GitFile {..}= do
  let path = P.encodeString (gitRepoPath repo) </> _fileBlobPath
  exists <- doesFileExist path
  unless exists $ do
    createDirectoryIfMissing True (dropFileName path)
    S.writeFile path _fileBlobRaw
  return _fileRef



persistDirectory :: Git -> Map.Map FileName (WorkTree Ref Ref) -> IO (WorkTree Ref Ref)
persistDirectory repo tree = do
  ref <- setObject repo $
    ObjTree $
    Tree
      [ (getTreeMode t, toEntName name, getTreeRef t)
      | (name, t) <- Map.toAscList tree ]
  return $ Directory ref tree    
  where
    toEntName (FileName fName) = entName fName
    toEntName (DirectoryName dirName) = entName $ S.init dirName


diveTreePersist :: Git -> WorkTree () GitFile -> IO (WorkTree Ref Ref)
diveTreePersist repo (File f t) = do
  ref <- persistGitFile repo f
  return $ File ref t
diveTreePersist repo (Directory _ dirMap) = do
  let dir = Map.toAscList dirMap
  persistedTrees <- mapM (diveTreePersist repo . snd) dir
  let persistedMap = Map.fromAscList $ zip (map fst dir) persistedTrees
  persistDirectory repo persistedMap
  

data GitObject = GitBlob GitFile
               | GitTree (WorkTree Ref Ref)

flushWorkTree :: Git -> WorkTree Ref Ref -> WorkTree () GitFile -> IO (WorkTree Ref Ref)
flushWorkTree repo curTree workTree = flushWorkTreeRec curTree workTree
  where
    flushWorkTreeRec (File ref _) file@(File f t2)
      | ref == _fileRef f = return $ File ref t2
      | otherwise = diveTreePersist repo file
    flushWorkTreeRec (Directory {}) file@(File _ _) = do
      diveTreePersist repo file
    flushWorkTreeRec (File {}) d2@(Directory _ _) = do
      diveTreePersist repo d2
    flushWorkTreeRec d1@(Directory _ dirMap1) (Directory _ dirMap2)
      {- Empty root case: unchanged -}
      | Map.null dirMap2 = return d1
      {- In case new directory is a subset of an old one, check whether the content
         has changed or not. -}
      | Map.null $ Map.difference dirMap2 dirMap1 = do
        let dirList1 = Map.toAscList $ Map.intersection dirMap1 dirMap2
            dirList2 = Map.toAscList dirMap2
        newDirList <-
          zipWithM flushWorkTreeRec (map snd dirList1) (map snd dirList2)
        -- if all files have same refs and modes: folder hasn't changed
        if map getTreeMode newDirList == map (getTreeMode . snd) dirList1 &&
           map getTreeRef newDirList == map (getTreeRef . snd) dirList1
          then return d1
          else do
            let newDirMap = Map.fromAscList $ zip (map fst dirList2) newDirList
            persistDirectory repo newDirMap
      {- At this point there definitely a change -}
      | otherwise = do
        let unchangedMap = Map.difference dirMap1 dirMap2
            newMap = Map.difference dirMap2 dirMap1
            newList = Map.toAscList newMap
        newFiles <- mapM (diveTreePersist repo . snd) newList
        let distinctMap =
              Map.union unchangedMap $ Map.fromAscList $ zip (map fst newList) newFiles
            oldDirs = Map.toAscList $ Map.difference dirMap1 unchangedMap
            changedDirs = Map.toAscList $ Map.difference dirMap2 newMap
        selectedDirs <-
          zipWithM flushWorkTreeRec (map snd oldDirs) (map snd changedDirs)
        let persistedMap =
              Map.union distinctMap $
              Map.fromAscList $ zip (map fst changedDirs) selectedDirs
        persistDirectory repo persistedMap
        
