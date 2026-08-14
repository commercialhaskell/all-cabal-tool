{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TUF-verified access to the Hackage package index and source tarballs, via
-- the @hackage-security@ client.
module Stackage.Package.Hackage
  ( Hackage
  , HasUpdates(..)
  , withHackage
  , checkIndexUpdates
  , indexTarPath
  , downloadSdist
  , sdistLocations
  ) where

import Control.Monad (when)
import Data.Time (UTCTime)
import Distribution.Package (PackageIdentifier(..))
import Distribution.Text (display)
import Network.URI (URI)
import Network.URI.Static (uri)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))

import Hackage.Security.Client
       (HasUpdates(..), KeyId(..), KeyThreshold(..), Repository, bootstrap,
        cabalCacheLayout, checkForUpdates, downloadPackage',
        hackageIndexLayout, hackageRepoLayout, requiresBootstrap,
        uncheckClientErrors)
import Hackage.Security.Client.Formats (Format(..))
import Hackage.Security.Client.Repository.Cache
       (Cache(..), getCachedIndex)
import qualified Hackage.Security.Client.Repository.Remote as Remote
import Hackage.Security.Util.Path (fromFilePath, makeAbsolute, toFilePath)
import Hackage.Security.Util.Pretty (pretty)

import Stackage.Package.HttpLib (withHttpLib)

-- | A bootstrapped connection to Hackage together with the local cache backing
-- it.
data Hackage = Hackage
  { hackageRepository :: Repository Remote.RemoteTemp
  , hackageCache :: Cache
  }

-- | Primary server first, then out-of-band mirrors.
--
-- Hackage's own @mirrors.json@ currently lists only defunct mirrors, so the
-- Haskell Foundation mirror has to be supplied out of band.
hackageMirrors :: [URI]
hackageMirrors =
  [ [uri|https://hackage.haskell.org/|]
  , [uri|https://hackage-mirror.haskell.foundation/|]
  ]

-- | The keys and threshold published in @hackage.haskell.org@'s @root.json@.
--
-- Pinning them here means the very first run establishes trust from this
-- source rather than from whatever the network happens to serve.
hackageRootKeyIds :: [KeyId]
hackageRootKeyIds =
  map
    KeyId
    [ "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
    , "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
    , "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
    , "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
    , "c7de58fc6a224b92b5b513f26fbb8b370f2d97c7cfe0075a951314a55734be93"
    , "d26e46f3b631aae1433b89379a6c68bd417eb5d1c408f0643dcc07757fece522"
    ]

hackageRootKeyThreshold :: KeyThreshold
hackageRootKeyThreshold = KeyThreshold 3

-- | Open the repository, bootstrapping the chain of trust into @cacheDir@ if it
-- is not there yet.
withHackage :: FilePath -> (Hackage -> IO a) -> IO a
withHackage cacheDir callback =
  withHttpLib $ \httpLib -> do
    createDirectoryIfMissing True cacheDir
    root <- makeAbsolute (fromFilePath cacheDir)
    let cache = Cache {cacheRoot = root, cacheLayout = cabalCacheLayout}
    Remote.withRepository
      httpLib
      hackageMirrors
      Remote.defaultRepoOpts
      cache
      hackageRepoLayout
      hackageIndexLayout
      (putStrLn . pretty)
      (\rep -> do
         uncheckClientErrors $ do
           needsBootstrap <- requiresBootstrap rep
           when needsBootstrap $
             bootstrap rep hackageRootKeyIds hackageRootKeyThreshold
         callback Hackage {hackageRepository = rep, hackageCache = cache})

-- | Refresh the cached index, reporting whether anything actually changed.
checkIndexUpdates
  :: Hackage
  -> UTCTime -- ^ Time to judge metadata expiry against
  -> IO HasUpdates
checkIndexUpdates hackage now =
  uncheckClientErrors $
    checkForUpdates (hackageRepository hackage) (Just now)

-- | Location of the uncompressed @01-index.tar@ left behind by
-- 'checkIndexUpdates'.
indexTarPath :: Hackage -> IO FilePath
indexTarPath hackage = do
  mpath <- getCachedIndex (hackageCache hackage) FUn
  case mpath of
    Just path -> return (toFilePath path)
    Nothing ->
      error "Stackage.Package.Hackage.indexTarPath: no index in the cache"

-- | Fetch a source tarball, verifying it against the index metadata.
downloadSdist :: Hackage -> PackageIdentifier -> FilePath -> IO ()
downloadSdist hackage pkgId dest =
  uncheckClientErrors $ downloadPackage' (hackageRepository hackage) pkgId dest

-- | Every mirror's URL for a source tarball, primary server first.
sdistLocations :: PackageIdentifier -> [String]
sdistLocations pkgId =
  [ show mirror </> "package" </> display pkgId ++ ".tar.gz"
  | mirror <- hackageMirrors
  ]
