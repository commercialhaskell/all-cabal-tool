{-# LANGUAGE RecordWildCards #-}

module Stackage.Package.Locations where

import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Version (Version)
import Distribution.Package (PackageName)
import Stackage.Package.IndexConduit (renderDistText)
import System.FilePath (dropFileName, (</>))
import System.Directory (createDirectoryIfMissing)

data HackageMirror = HackageMirror
  { baseHackageUrl :: String
  }

hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"

hackageDeprecatedUrl :: String
hackageDeprecatedUrl = hackageBaseUrl ++ "/packages/deprecated.json"

hackagePreferredUrl :: String
hackagePreferredUrl = hackageBaseUrl ++ "/packages/preferred-versions"

mirrorFPCompleteHTTP :: HackageMirror
mirrorFPCompleteHTTP = HackageMirror "https://hackage.haskell.org"

mirrorFPCompleteHTTPS :: HackageMirror
mirrorFPCompleteHTTPS =
  HackageMirror "https://s3.amazonaws.com/hackage.fpcomplete.com"

hackageMirror :: HackageMirror
hackageMirror = HackageMirror hackageBaseUrl

getUrlIndexFile00 :: HackageMirror -> String
getUrlIndexFile00 HackageMirror {..} = baseHackageUrl ++ "/00-index.tar.gz"

getUrlIndexFile01 :: HackageMirror -> String
getUrlIndexFile01 HackageMirror {..} = baseHackageUrl ++ "/01-index.tar.gz"

getUrlPackageFile :: HackageMirror -> PackageName -> Version -> String
getUrlPackageFile HackageMirror {..} pkgName pkgVer =
  intercalate
    "/"
    [ baseHackageUrl
    , "package"
    , concat [renderDistText pkgName, "-", renderDistText pkgVer, ".tar.gz"]
    ]

data GitRepository = GitRepository
  { repoAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , repoLocalPath :: FilePath
    -- ^ Filepath to the root of the repository.
  } deriving (Show)

data Repositories = Repositories
  { allCabalFiles :: GitRepository
  , allCabalHashes :: GitRepository
  , allCabalMetadata :: GitRepository
  } deriving (Show)

-- | TODO: Clones the repo if it doesn't exists locally yet, otherwise pulls from it.
ensureGitRepository
  :: String -- ^ Git provider ex. "github.com"
  -> String -- ^ Repository account ex. "commercialhaskell"
  -> String -- ^ Repository name ex. "all-cabal-files"
  -> FilePath -- ^ Location in the file system where
     -- repository should be cloned to.
  -> IO GitRepository
ensureGitRepository repoHost repoAccount repoName repoBasePath = do
  return repo
  where
    repo =
      GitRepository
      { repoAddress = repoAddress'
      , repoLocalPath = repoLocalPath'
      }
    repoLocalPath' = repoBasePath </> repoName
    repoAddress' =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]


-- | Writes a file to the repository folder. Path is created if it doesn't
-- exist.
repoFileWriter
  :: GitRepository -- ^ Repository the file should be written to.
  -> FilePath -- ^ Paths relative to the root of the repository.
  -> (FilePath -> IO ()) -- ^ Function that should be used to write
     -- the file.
  -> IO ()
repoFileWriter repo fp writer = do
  let fullPath = repoLocalPath repo </> fp
  createDirectoryIfMissing True (dropFileName fullPath)
  writer fullPath


-- | Reads a file from the repository folder
repoFileReader
  :: GitRepository -- ^ Repository the file should be read from.
  -> FilePath -- ^ Paths relative to the root of the repository.
  -> (FilePath -> IO a) -- ^ Function that should be used to write
     -- the file.
  -> IO a
repoFileReader repo fp = ($ repoLocalPath repo </> fp)
