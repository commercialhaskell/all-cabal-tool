-- | Exercises the @hackage-security@ client against the live Hackage
-- repository: bootstrapping trust, fetching the index, and fetching both a
-- verified source tarball and a withdrawn one.
--
-- Enable with @cabal test --flags=integration@.
module Main where

import Control.Monad (unless)
import Data.Time (getCurrentTime)
import Distribution.Package (PackageIdentifier(..), mkPackageName)
import Distribution.Version (mkVersion)
import System.Directory (getFileSize)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.IO.Temp (withSystemTempDirectory)

import Stackage.Package.Hackage

-- | @text-2.1.2@ is long since released, so its tarball is immutable.
sampleSdist :: (PackageIdentifier, Integer)
sampleSdist =
  (PackageIdentifier (mkPackageName "text") (mkVersion [2, 1, 2]), 449871)

-- | @hermes-1.3.4.3@ is still named by the index, but isn't available.
withdrawnSdist :: PackageIdentifier
withdrawnSdist =
  PackageIdentifier (mkPackageName "hermes") (mkVersion [1, 3, 4, 3])

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  withSystemTempDirectory "all-cabal-tool-integration" $ \cacheDir ->
    withHackage cacheDir $ \hackage -> do
      now <- getCurrentTime
      (hasUpdates, indexPath) <- refreshIndex hackage now
      check "a fresh cache reports updates" $
        case hasUpdates of
          HasUpdates -> True
          NoUpdates -> False
      indexSize <- getFileSize indexPath
      check "the index was cached" (indexSize > 0)
      let (pkgId, expectedSize) = sampleSdist
      fetched <- withSdist hackage pkgId getFileSize
      check "the source tarball has the published size" $
        case fetched of
          Right sdistSize -> sdistSize == expectedSize
          Left _ -> False
      withdrawn <- withSdist hackage withdrawnSdist getFileSize
      check "a withdrawn source tarball is reported, not thrown" $
        case withdrawn of
          Left _ -> True
          Right _ -> False

check :: String -> Bool -> IO ()
check what ok = do
  putStrLn $ (if ok then "PASS: " else "FAIL: ") ++ what
  unless ok exitFailure
