{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Stackage.Package.Git.Types (FileName(..), TreePath)

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

  shrink (FileName bs) =
    [ FileName (SBS.toShort $ B.pack shorter)
    | let chars = B.unpack $ SBS.fromShort bs
    , shorter <- shrinkList (const []) chars
    , not (null shorter)
    ]
  shrink (DirectoryName bs) =
    -- Remove the trailing slash, shrink the name, add slash back
    [ DirectoryName (SBS.toShort $ B.pack $ shorter ++ [slash])
    | let chars = init $ B.unpack $ SBS.fromShort bs  -- remove trailing /
    , shorter <- shrinkList (const []) chars
    , not (null shorter)
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

  shrink (TestTreePath path) =
    [ TestTreePath p
    | p <- shrinkList (const []) path
    , not (null p)
    , isValidPath p
    ]
    where
      isValidPath [] = False
      isValidPath xs = case last xs of
        FileName _ -> True
        DirectoryName _ -> False

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
  ]
