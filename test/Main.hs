{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Git.Ref (Ref, fromBinary, toBinary)

import Stackage.Package.Git.Object (makeGitFile)
import Stackage.Package.Git.Types (FileName(..), GitFile(..), TreePath, toShortRef, fromShortRef)
import Stackage.Package.Git.WorkTree (emptyWorkTree, insertGitFile, lookupFile, removeGitFile)

-- | Arbitrary instance for FileName.
-- Generates valid file/directory names (non-empty, no slashes in names).
instance Arbitrary FileName where
  arbitrary = oneof
    [ FileName <$> genShortBS
    , DirectoryName . addSlash <$> genShortBS
    ]
    where
      genShortBS :: Gen SBS.ShortByteString
      genShortBS = do
        -- Generate 1-20 alphanumeric characters
        len <- choose (1, 20)
        chars <- vectorOf len $ elements validChars
        return $ SBS.toShort $ B.pack chars

      validChars :: [Word8]
      validChars = map (fromIntegral . fromEnum) $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '.', '_']

      addSlash :: SBS.ShortByteString -> SBS.ShortByteString
      addSlash sbs = SBS.toShort $ B.snoc (SBS.fromShort sbs) slash

      slash :: Word8
      slash = fromIntegral (fromEnum '/')

  -- Shrink to smaller prefixes, smallest first
  shrink (FileName bs) =
    let chars = B.unpack $ SBS.fromShort bs
    in [ FileName (SBS.toShort $ B.pack $ take n chars)
       | n <- [1 .. length chars - 1]
       ]
  -- Shrink to smaller prefixes + slash, smallest first
  shrink (DirectoryName bs) =
    let chars = B.unpack $ SBS.fromShort bs
        name = init chars  -- remove trailing /
    in [ DirectoryName (SBS.toShort $ B.pack $ take n name ++ [slash])
       | n <- [1 .. length name - 1]
       ]
    where
      slash :: Word8
      slash = fromIntegral (fromEnum '/')

-- | Arbitrary instance for TreePath.
newtype TestTreePath = TestTreePath { unTestTreePath :: TreePath }
  deriving (Show, Eq)

instance Arbitrary TestTreePath where
  arbitrary = do
    -- Generate 1-5 path components, ending with a FileName
    len <- choose (1, 5)
    if len == 1
      then do
        name <- genFileName
        return $ TestTreePath [name]
      else do
        dirs <- vectorOf (len - 1) genDirName
        file <- genFileName
        return $ TestTreePath (dirs ++ [file])
    where
      genShortBS :: Gen SBS.ShortByteString
      genShortBS = do
        len <- choose (1, 10)
        chars <- vectorOf len $ elements validChars
        return $ SBS.toShort $ B.pack chars

      validChars :: [Word8]
      validChars = map (fromIntegral . fromEnum) $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '.']

      genFileName :: Gen FileName
      genFileName = FileName <$> genShortBS

      genDirName :: Gen FileName
      genDirName = DirectoryName . addSlash <$> genShortBS

      addSlash :: SBS.ShortByteString -> SBS.ShortByteString
      addSlash sbs = SBS.toShort $ B.snoc (SBS.fromShort sbs) slash

      slash :: Word8
      slash = fromIntegral (fromEnum '/')

  -- Shrink to shorter paths, smallest first
  -- A valid path must end with a FileName, so we keep the last element
  -- and shrink by removing directories from the front
  shrink (TestTreePath path) =
    case path of
      [] -> []
      [_] -> []  -- Single element, can't shrink further
      _ ->
        let file = last path
            dirs = init path
        in [ TestTreePath (drop n dirs ++ [file])
           | n <- [length dirs, length dirs - 1 .. 1]
           ]

-- FileName properties

-- | Eq reflexivity: x == x
prop_eq_reflexive :: FileName -> Bool
prop_eq_reflexive x = x == x

-- | Ord totality: either x <= y or y <= x
prop_ord_total :: FileName -> FileName -> Bool
prop_ord_total x y = x <= y || y <= x

-- | Ord transitivity: if x <= y and y <= z then x <= z
prop_ord_transitive :: FileName -> FileName -> FileName -> Property
prop_ord_transitive x y z =
  (x <= y && y <= z) ==> x <= z

-- | Directory sorting quirk: DirectoryName "foo-0.1/" > DirectoryName "foo-0.1.1/"
-- This tests the git-specific sorting behavior where the trailing slash affects ordering.
prop_directory_sorting :: Property
prop_directory_sorting =
  let d1 = DirectoryName "foo-0.1/"
      d2 = DirectoryName "foo-0.1.1/"
  in property $ d1 > d2

-- | FileName and DirectoryName with same base name should be equal
prop_file_dir_equality :: Property
prop_file_dir_equality =
  let f = FileName "foo"
      d = DirectoryName "foo/"
  in property $ f == d

-- WorkTree properties

-- | Arbitrary instance for generating file content for GitFile
newtype TestContent = TestContent { unTestContent :: B.ByteString }
  deriving (Show, Eq)

instance Arbitrary TestContent where
  arbitrary = do
    len <- choose (0, 100)
    bytes <- vectorOf len arbitrary
    return $ TestContent $ B.pack bytes

  -- Shrink to smaller prefixes, smallest (empty) first
  shrink (TestContent bs) =
    let chars = B.unpack bs
    in [ TestContent (B.pack $ take n chars)
       | n <- [0 .. length chars - 1]
       ]

-- | Helper to create a GitFile from ByteString content
mkTestGitFile :: B.ByteString -> IO GitFile
mkTestGitFile content = do
  let lbs = B.fromStrict content
      sz = fromIntegral $ B.length content
  makeGitFile lbs sz

-- | Insert then lookup roundtrip: inserting a file and looking it up should return the same file
-- We compare by gitFileRef since GitFile doesn't have an Eq instance.
prop_insert_lookup :: TestTreePath -> TestContent -> Property
prop_insert_lookup (TestTreePath path) (TestContent content) = monadicIO $ do
  gitFile <- run $ mkTestGitFile content
  let tree = insertGitFile emptyWorkTree path gitFile
      result = lookupFile tree path
  return $ fmap gitFileRef result == Just (gitFileRef gitFile)

-- | Insert then remove: looking up a removed file should return Nothing
prop_insert_remove :: TestTreePath -> TestContent -> Property
prop_insert_remove (TestTreePath path) (TestContent content) = monadicIO $ do
  gitFile <- run $ mkTestGitFile content
  let tree = insertGitFile emptyWorkTree path gitFile
      tree' = removeGitFile tree path
      result = lookupFile tree' path
  return $ case result of
    Nothing -> True
    Just _  -> False

-- ShortRef properties

-- | Arbitrary Ref: generate 20 random bytes and convert to Ref
newtype TestRef = TestRef { unTestRef :: Ref }
  deriving (Show, Eq)

instance Arbitrary TestRef where
  arbitrary = do
    -- SHA1 is 20 bytes
    bytes <- vectorOf 20 arbitrary
    return $ TestRef $ fromBinary $ B.pack bytes

  -- Shrink by zeroing bytes from the end, smallest first
  shrink (TestRef ref) =
    let bytes = B.unpack $ toBinary ref
    in [ TestRef $ fromBinary $ B.pack $ take n bytes ++ replicate (20 - n) 0
       | n <- [0 .. length bytes - 1]
       ]

-- | ShortRef roundtrip: fromShortRef . toShortRef == id
prop_shortref_roundtrip :: TestRef -> Property
prop_shortref_roundtrip (TestRef ref) = monadicIO $ do
  shortRef <- run $ toShortRef ref
  let ref' = fromShortRef shortRef
  return $ ref' == ref

-- GitFile properties

-- | makeGitFile is deterministic: same content produces same ref
prop_makegitfile_deterministic :: TestContent -> Property
prop_makegitfile_deterministic (TestContent content) = monadicIO $ do
  gitFile1 <- run $ mkTestGitFile content
  gitFile2 <- run $ mkTestGitFile content
  return $ gitFileRef gitFile1 == gitFileRef gitFile2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all-cabal-tool"
  [ testGroup "FileName"
    [ testProperty "Eq reflexive" prop_eq_reflexive
    , testProperty "Ord total" prop_ord_total
    , testProperty "Ord transitive" prop_ord_transitive
    , testProperty "directory sorting quirk" prop_directory_sorting
    , testProperty "file/dir equality" prop_file_dir_equality
    ]
  , testGroup "WorkTree"
    [ testProperty "insert-lookup roundtrip" prop_insert_lookup
    , testProperty "insert-remove" prop_insert_remove
    ]
  , testGroup "ShortRef"
    [ testProperty "roundtrip" prop_shortref_roundtrip
    ]
  , testGroup "GitFile"
    [ testProperty "makeGitFile deterministic" prop_makegitfile_deterministic
    ]
  ]
