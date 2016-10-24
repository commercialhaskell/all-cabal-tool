{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Package.Git.Object where

import ClassyPrelude.Conduit
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteArray (convert)
import Crypto.Hash (SHA1(..), Digest)
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Zlib (compress, defaultWindowBits)
import qualified Data.Git.Types as G
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Git.Ref
import Data.Git.Storage.Loose
import System.Directory
import System.FilePath


import Stackage.Package.Git.Types



data GitObject
  = Blob GitFile
  | Tree GitTree
  | Commit GitCommit
  | Tag GitTag

makeGitFile
  :: (MonadThrow m, PrimMonad base, MonadBase base m) =>
     LByteString -> Word64 -> m GitFile
makeGitFile lbs sz = do
  --let content = looseMarshall (ObjBlob (G.Blob lbs))
  (sha1, zipped) <- runConduit $ srcWithHeader "blob" lbs sz
    =$= getZipSink ((,) <$> ZipSink sha1Sink <*> ZipSink compressSink)
  return $
    GitFile
    { gitFileRef = unDigestRef sha1
    , gitFileType = RegularFile NonExecFile
    , gitFileZipped = zipped
    }


-- | Creates a simplified git commit.
makeGitCommit :: Ref -> [Ref] -> GitUser -> ByteString -> IO GitCommit
makeGitCommit treeRef parents user commitMessage = do
  person <- getPerson user
  return $
    G.Commit
    { G.commitTreeish = treeRef
    , G.commitParents = parents
    , G.commitAuthor = person
    , G.commitCommitter = person
    , G.commitEncoding = Nothing
    , G.commitExtras = []
    , G.commitMessage = commitMessage
    }



-- | Creates a tag of a commit.
makeGitTag :: Ref -> GitUser -> ByteString -> ByteString -> IO GitTag
makeGitTag commitRef gitUser tagStr tagMessage = do
  person <- getPerson gitUser
  return $
    G.Tag
    { G.tagRef = commitRef
    , G.tagObjectType = TypeCommit
    , G.tagBlob = tagStr
    , G.tagName = person
    , G.tagS = tagMessage
    }


-- TODO: Switch to conduit ref calculation and compression.


marshallObject :: GitObject -> (Ref, LByteString)
marshallObject (Blob f) = (gitFileRef f, L.fromStrict $ gitFileZipped f)
marshallObject (Tree o) = (hashLBS content, Zlib.compress content)
  where content = looseMarshall $ ObjTree o
marshallObject (Commit o) = (hashLBS content, Zlib.compress content)
  where content = looseMarshall $ ObjCommit o
marshallObject (Tag o) = (hashLBS content, Zlib.compress content)
  where content = looseMarshall $ ObjTag o


srcWithHeader
  :: (Monad m)
  => ByteString -> L.ByteString -> Word64 -> ConduitM i ByteString m ()
srcWithHeader oType oContent oSize =
  sourceLazy $
  L.append (L.fromChunks [oType, " ", S8.pack $ show oSize, S.singleton 0]) oContent


compressSink
  :: (MonadBase base m, PrimMonad base, MonadThrow m)
  => Consumer ByteString m ByteString
compressSink = compress 1 defaultWindowBits =$= foldC


sha1Sink :: (Monad m) => Consumer ByteString m (Digest SHA1)
sha1Sink = sinkHash


unDigestRef :: Digest SHA1 -> Ref
unDigestRef = fromBinary . convert


repoWriteObject :: GitRepository -> GitObject -> IO Ref
repoWriteObject GitRepository {repoInstance = GitInstance {..}
                              ,repoInfo = GitInfo {..}} obj = do
  let (ref, zipped) = marshallObject obj
  loc <- findReference gitRepo ref
  case loc of
    NotFound -> do
      let (prefix, suffix) = toFilePathParts ref
          path = gitLocalPath </> "objects" </> prefix </> suffix
      exists <- doesFileExist path
      unless exists $
        do createDirectoryIfMissing True (dropFileName path)
           writeFile path zipped
    _ -> return ()
  return ref
