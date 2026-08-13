{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stackage.Package.Hashes where
import ClassyPrelude.Conduit
import Crypto.Hash
       (Digest, HashAlgorithm, MD5(..), SHA1(..), SHA256(..), SHA512(..),
        Skein512_512(..))
import Crypto.Hash.Conduit (sinkHash)
import Data.Aeson
       (FromJSON(..), ToJSON(..), eitherDecode', encode, object,
        withObject, (.:), (.:?), (.=))
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Version (Version)
import Distribution.Package (PackageName)
import Distribution.Package (PackageIdentifier(..))
import System.FilePath (dropExtension)
import Data.Text.IO (hPutStrLn)

import Stackage.Package.Git
import Stackage.Package.Hackage
import Stackage.Package.IndexConduit


-- | Compares hashes in 'package.json' to the ones in the repo. In case a new
-- package.json appears without coresponding hashes in the repo, a package is
-- downloaded, hashes are computed and compared to ones in 'package.json' file.
-- Returned is a map with all packages and their valid versions.
sinkPackageHashes
  :: MonadIO m
  => Hackage
  -> GitRepository
  -> ConduitT IndexEntry Void m (Map PackageName (Set Version))
sinkPackageHashes hackage hashesRepo = CL.foldM updateHashes Map.empty
  where
    updateHashes versionsMap (PackageEntry IndexFile { ifFile = HackagePackage {..}
                                                     , ..
                                                     }) = do
      validHashes <-
        createHashesIfMissing
          hackage
          hashesRepo
          (hHashes hackageHashes)
          ifPackageName
          hackageVersion
      return $
        case validHashes of
          False -> versionsMap
          True ->
            let with =
                  Just .
                  maybe
                    (Set.singleton hackageVersion)
                    (Set.insert hackageVersion)
            in Map.alter with ifPackageName versionsMap
    updateHashes versionsMap _ = return versionsMap



-- | Checks whether hashes file exists for specific package version.
containsHashesFor :: Map PackageName (Set Version) -> PackageName -> Version -> Bool
containsHashesFor versionsMap pkgName pkgVersion =
  maybe False (Set.member pkgVersion) $ lookup pkgName versionsMap

-- | Validates hashes against @package.json@ file.
validateHackageHashes :: (MonadIO m, Eq a) =>
                         Text -- ^ Package name
                      -> Map Text a -- ^ Map with hashes from Hackage
                      -> Map Text a -- ^ Map with hashes from all-cabal-hashes
                      -> m Bool
validateHackageHashes packageName hackageHashesMap packageHashesMap =
  liftIO $ fmap and $
  forM [tshow MD5, tshow SHA256] $ \hashType -> do
    let isValid =
          lookup (toLower hashType) hackageHashesMap ==
          lookup hashType packageHashesMap
    unless
      isValid
      (hPutStrLn stderr $
       "Stackage.Hackage.Hashes.entryUpdateHashes: Hash " ++
       hashType ++
       "value mismatch for: '" ++
       packageName ++ "' computed vs one from Hackage.")
    return isValid


-- | If json file with package hashes is missing or corrupt (not parsable) it
-- downloads the taralls with source code and saves their the hashes.
createHashesIfMissing
  :: MonadIO m
  => Hackage
  -> GitRepository
  -> Map Text Text -- ^ Map with hashes from Hackage
  -> PackageName
  -> Version
  -> m Bool
createHashesIfMissing hackage hashesRepo hackageHashMap pkgName pkgVersion =
  liftIO $ do
    let jsonfp = dropExtension (getCabalFilePath pkgName pkgVersion) <.> "json"
    meres <- fmap eitherDecode' <$> repoReadFile hashesRepo jsonfp
    let mpackageHashes =
          case meres of
            (Just (Left e)) ->
              error $ concat ["Could not parse ", jsonfp, ": ", e]
            (Just (Right x)) -> flatten x
            _ -> Nothing
    case mpackageHashes of
      Just package ->
        validateHackageHashes
          (pack $ getPackageFullName pkgName pkgVersion)
          hackageHashMap
          (packageHashes package)
      Nothing -> do
        package <- computePackage hackage pkgName pkgVersion
        areAllValid <-
          validateHackageHashes
            (pack $ getPackageFullName pkgName pkgVersion)
            hackageHashMap
            (packageHashes package)
        when areAllValid $ repoWriteFile hashesRepo jsonfp (encode package)
        return areAllValid

-- | Kinda like sequence, except not.
flatten :: Package Maybe -> Maybe (Package Identity)
flatten (Package h l ms) = Package h l . Identity <$> ms

data Package f = Package
  { packageHashes    :: Map Text Text
  , packageLocations :: [Text]
  , packageSize      :: f Word64
  }

instance ToJSON (Package Identity) where
  toJSON (Package h l (Identity s)) =
    object
      ["package-hashes" .= h, "package-locations" .= l, "package-size" .= s]

instance FromJSON (Package Maybe) where
  parseJSON =
    withObject "Package" $
    \o ->
       Package <$> o .: "package-hashes" <*> o .: "package-locations" <*>
       o .:? "package-size"

-- | Fetch a source tarball and derive its hashes and size.
computePackage
  :: MonadIO m
  => Hackage
  -> PackageName -- ^ Package name
  -> Version -- ^ Package version
  -> m (Package Identity)
computePackage hackage pkgName pkgVersion = liftIO $ do
  putStrLn $ "Computing package information for: " ++ pack pkgFullName
  (hashes, size) <-
    withSdist hackage pkgId $ \path -> do
      lbs <- L.readFile path
      runConduit $ CL.sourceList (L.toChunks lbs) .| getZipSink pairSink
  return
    Package
    { packageHashes = hashes
    , packageLocations = map pack (sdistLocations pkgId)
    , packageSize = Identity size
    }
  where
    pkgId = PackageIdentifier pkgName pkgVersion
    pkgFullName = getPackageFullName pkgName pkgVersion
    pairSink = (,) <$> hashesSink <*> ZipSink lengthCE
    hashesSink =
      fmap unions $
      sequenceA
        [ mkSink SHA1
        , mkSink SHA256
        , mkSink SHA512
        , mkSink Skein512_512
        , mkSink MD5
        ]

mkSink
  :: (Monad m, Show hash, HashAlgorithm hash)
  => hash -> ZipSink ByteString m (Map Text Text)
mkSink ha =
  ZipSink $
  do digest <- sinkHash
     return $ singletonMap (tshow ha) $ unDigest ha digest

unDigest
  :: HashAlgorithm hash
  => hash -> Digest hash -> Text
unDigest _ = decodeUtf8 . convertToBase Base16
