{-# LANGUAGE RecordWildCards #-}

module Stackage.Package.Locations where

import System.FilePath (dropFileName, (</>))
import System.Directory (createDirectoryIfMissing)


hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"


hackageDeprecatedUrl :: String
hackageDeprecatedUrl = hackageBaseUrl ++ "/packages/deprecated.json"


mirrorFPComplete :: String
mirrorFPComplete = "https://s3.amazonaws.com/hackage.fpcomplete.com"


data GitUser = GitUser
  { userName :: String
  , userEmail :: String
  , userGPG :: String
  } deriving (Show)



data GitRepository = GitRepository
  { repoAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , repoBranch :: String
    -- ^ Branch that updates should be committed to.
  , repoUser :: GitUser
    -- ^ User information to be used for the commits.
  , repoTag :: Maybe String
    -- ^ Create a tag after an update.
  , repoLocalPath :: FilePath
    -- ^ Filepath to the root of the repository.
  } deriving (Show)

data Repositories = Repositories
  { allCabalFiles :: GitRepository
  , allCabalHashes :: GitRepository
  , allCabalMetadata :: GitRepository
  } deriving (Show)


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
