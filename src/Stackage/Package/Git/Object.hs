{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Git.Object
  ( GitObject(..)
  , makeGitFile
  , makeGitCommit
  , makeGitTag
  , repoWriteObject
  ) where

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


-- | An an object that can be written to git store.
data GitObject
  = Blob GitFile
  | Tree GitTree
  | Commit GitCommit
  | Tag GitTag


-- | Marshalls a bytestring into a git blob object, computes its SHA1,
-- compresses it and returns a `GitFile` that is of type `NonExecFile`.
makeGitFile
  :: (MonadThrow m, PrimMonad m)
  => LByteString -- ^ Content of the blob.
  -> Word64 -- ^ Size of the content.
  -> m GitFile
makeGitFile lbs sz = do
  (sha1, zipped) <-
    runConduit $
    srcWithHeader "blob" lbs sz =$=
    getZipSink ((,) <$> ZipSink sha1Sink <*> ZipSink compressSink)
  unless (sz == fromIntegral (L.length lbs)) $
    error "Stackage.Package.Git.makeGitFile: Size mismatch."
  return $
    GitFile
    { gitFileRef = unDigestRef sha1
    , gitFileType = NonExecFile
    , gitFileZipped = zipped
    }


-- | Creates a git commit.
makeGitCommit :: Ref -- ^ Root tree SHA1
              -> [Ref] -- ^ Parent commits SHA1
              -> GitUser -- ^ Git user
              -> ByteString -- ^ Commit message
              -> IO GitCommit
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
makeGitTag :: Ref -- ^ Commit SHA1
           -> GitUser -- ^ Git user
           -> ByteString -- ^ Tag name
           -> ByteString -- ^ Tag message
           -> IO GitTag
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
srcWithHeader oType oContent oSize = do
  yield oType
  yield " "
  yield $ S8.pack $ show oSize
  yield $ S.singleton 0
  sourceLazy oContent


compressSink
  :: (PrimMonad m, MonadThrow m)
  => Consumer ByteString m ByteString
compressSink = compress 1 defaultWindowBits =$= foldC


sha1Sink :: (Monad m) => Consumer ByteString m (Digest SHA1)
sha1Sink = sinkHash


unDigestRef :: Digest SHA1 -> Ref
unDigestRef = fromBinary . convert


-- | Writes an object to the disk as a loose object, unless it already exists in
-- the git repository.
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
           L.writeFile path zipped
    _ -> return ()
  return ref
