{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage.Package.Git.WorkTree where

import ClassyPrelude.Conduit
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Byteable
import qualified Data.Git as G
import Data.Git.Ref
import Data.Git.Storage
import qualified Data.Map.Strict as Map

import Stackage.Package.Git.Types
import Stackage.Package.Git.Object


directoryToTree :: Map.Map FileName (WorkTree Ref Ref) -> G.Tree
directoryToTree dirMap =
  G.Tree
    [ (getTreeMode t, toEntName name, getTreeRef t)
    | (name, t) <- Map.toAscList dirMap ]
  where
    toEntName (FileName fName) = G.entName fName
    toEntName (DirectoryName dirName) = G.entName $ S.init dirName


emptyWorkTree :: WorkTree () GitFile
emptyWorkTree = Directory () Map.empty



insertGitFile :: WorkTree () GitFile -> TreePath -> GitFile -> WorkTree () GitFile
insertGitFile tree path f = insertFileRec tree path
  where
    insertFileRec _ [] = error "Cannot insert a file without a name"
    insertFileRec (Directory _ dirMap) [fName] =
      Directory () $ Map.insert fName (File f $ gitFileType f) dirMap
    insertFileRec (Directory _ dirMap) (dirName:pathTail) = Directory () newDirMap
      where
        newDirMap =
          case Map.lookup dirName dirMap of
            Just subTree@(Directory _ _) ->
              Map.insert dirName (insertFileRec subTree pathTail) dirMap
            _ ->
              Map.insert dirName (insertFileRec (Directory () Map.empty) pathTail) dirMap
    insertFileRec File {} treePath = insertFileRec (Directory () Map.empty) treePath


lookupFile :: WorkTree a f -> TreePath -> Maybe f
lookupFile tree path = getFile $ lookupRec tree path
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


removeGitFile :: WorkTree () GitFile -> TreePath -> WorkTree () GitFile
removeGitFile tree path = removeRec tree path
  where
    removeRec _ [] = error "Cannot remove a file without a name."
    removeRec (Directory _ dirMap) [fileName] =
      Directory () $ Map.update (const Nothing) fileName dirMap
    removeRec dir@(Directory _ dirMap) (dirName:pathTail) = newDir
      where
        newDir =
          case Map.lookup dirName dirMap of
            Just subTree -> removeRec subTree pathTail
            Nothing -> dir
    removeRec f@(File {}) _ = f


readWorkTree :: Git -> Ref -> IO (WorkTree Ref Ref)
readWorkTree repo rootRef = readTreeRec rootRef
  where
    readTreeFile (G.ModePerm mode, ent, ref)
      | mode == 0o120000 = return (FileName $ toBytes ent, File ref SymLink)
      | mode == 0o160000 = return (FileName $ toBytes ent, File ref GitLink)
      | (mode .&. 0o777000) == 0o100000 =
        return
          (FileName $ toBytes ent, File ref (RegularFile (toUnixPerm mode)))
      | mode == 0o040000 = do
        directory <- readTreeRec ref
        return (DirectoryName $ S8.snoc (toBytes ent) '/', directory)
      | otherwise = error $ "Unsupported file mode: " ++ show mode
    readTreeRec ref = do
      (G.Tree tree) <- G.getTree repo ref
      files <- mapM readTreeFile tree
      return $ Directory ref $ Map.fromAscList files


persistGitTree
  :: GitRepository
  -> Map.Map FileName (WorkTree G.Ref G.Ref)
  -> IO (WorkTree Ref Ref)
persistGitTree repo dirMap = do
  let tree = directoryToTree dirMap
  ref <- repoWriteObject repo (Tree tree)
  return $ Directory ref dirMap


diveTreePersist :: GitRepository -> WorkTree () GitFile -> IO (WorkTree Ref Ref)
diveTreePersist repo (File f t) = do
  ref <- repoWriteObject repo (Blob f)
  return $ File ref t
diveTreePersist repo (Directory _ dirMap) = do
  let dir = Map.toAscList dirMap
  persistedTrees <- mapM (diveTreePersist repo . snd) dir
  let persistedMap = Map.fromAscList $ zip (map fst dir) persistedTrees
  persistGitTree repo persistedMap


flushWorkTree :: GitRepository -> IO (Maybe Ref)
flushWorkTree repo@GitRepository {repoInstance = GitInstance {..}} = do
  modifyMVar gitWorkTree $ \workTree -> do
    mnewRootRef <- modifyMVar gitRootTree $ \rootTree -> do
      newRootTree <- flushWorkTreeRec rootTree workTree
      if getTreeRef newRootTree == getTreeRef rootTree
        then return (newRootTree, Nothing)
        else return (newRootTree, Just $ getTreeRef newRootTree)
    return (emptyWorkTree, mnewRootRef)
  
  where
    flushWorkTreeRec (File ref _) file@(File f t2)
      | ref == gitFileRef f = return $ File ref t2
      | otherwise = diveTreePersist repo file
    flushWorkTreeRec (Directory {}) file@(File _ _) = do
      diveTreePersist repo file
    flushWorkTreeRec (File {}) d2@(Directory _ _) = do
      diveTreePersist repo d2
    flushWorkTreeRec d1@(Directory _ dirMap1) (Directory _ dirMap2)
      {- Empty folder: unchanged -}
      | Map.null dirMap2 = return d1
      | otherwise = do
        let unchangedMap = Map.difference dirMap1 dirMap2
            newMap = Map.difference dirMap2 dirMap1
            newList = Map.toAscList newMap
        newFiles <- mapM (diveTreePersist repo . snd) newList
        let distinctMap =
              Map.union unchangedMap $ Map.fromAscList $ zip (map fst newList) newFiles
            oldDirs = Map.toAscList $ Map.difference dirMap1 unchangedMap
            changedDirs = Map.toAscList $ Map.difference dirMap2 newMap
        commonDirs <-
          zipWithM flushWorkTreeRec (map snd oldDirs) (map snd changedDirs)
        let newDirMap =
              Map.union distinctMap $
              Map.fromAscList $ zip (map fst changedDirs) commonDirs
        persistGitTree repo newDirMap
        
