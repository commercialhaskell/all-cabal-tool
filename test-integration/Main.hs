-- | Exercises the @hackage-security@ client against the live Hackage
-- repository: bootstrapping trust, fetching the index, and fetching a
-- verified source tarball.
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  withSystemTempDirectory "all-cabal-tool-integration" $ \cacheDir ->
    withHackage cacheDir $ \hackage -> do
      now <- getCurrentTime
      hasUpdates <- checkIndexUpdates hackage now
      check "a fresh cache reports updates" $
        case hasUpdates of
          HasUpdates -> True
          NoUpdates -> False
      indexSize <- getFileSize =<< indexTarPath hackage
      check "the index was cached" (indexSize > 0)
      let (pkgId, expectedSize) = sampleSdist
      sdistSize <- withSdist hackage pkgId getFileSize
      check "the source tarball has the published size" (sdistSize == expectedSize)

check :: String -> Bool -> IO ()
check what ok = do
  putStrLn $ (if ok then "PASS: " else "FAIL: ") ++ what
  unless ok exitFailure
