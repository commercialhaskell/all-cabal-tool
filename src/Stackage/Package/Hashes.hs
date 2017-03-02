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
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Version (Version)
import Distribution.Package (PackageName(..))
import Network.HTTP.Client.Conduit
       (HttpException(StatusCodeException), parseRequest,
        responseCookieJar, responseHeaders, responseStatus)
import Network.HTTP.Simple (httpSink)
import Network.HTTP.Types (statusCode)
import System.FilePath (dropExtension)

import Stackage.Package.Git
import Stackage.Package.IndexConduit
import Stackage.Package.Locations


-- | Compares hashes in 'package.json' to the ones in the repo. In case a new
-- package.json appears without coresponding hashes in the repo, a package is
-- downloaded, hashes are computed and compared to ones in 'package.json' file.
-- Returned is a map with all packages and their valid versions.
sinkPackageHashes
  :: (MonadIO m, MonadMask m)
  => GitRepository
  -> Consumer IndexEntry m (Map PackageName (Set Version))
sinkPackageHashes hashesRepo = CL.foldM updateHashes Map.empty
  where
    updateHashes versionsMap (PackageEntry IndexFile { ifFile = HackagePackage {..}
                                                     , ..
                                                     }) = do
      validHashes <-
        createHashesIfMissing
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
  fmap and $
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
  :: (MonadMask m, MonadIO m)
  => GitRepository
  -> Map Text Text -- ^ Map with hashes from Hackage
  -> PackageName
  -> Version
  -> m Bool
createHashesIfMissing hashesRepo hackageHashMap pkgName pkgVersion =
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
        mpackageComputed <- computePackage pkgName pkgVersion
        case mpackageComputed of
          Nothing -> return False
          Just package -> do
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

computePackage
  :: (MonadMask m, MonadIO m)
  => PackageName -- ^ Package name
  -> Version -- ^ Package version
  -> m (Maybe (Package Identity))
computePackage pkgName pkgVersion = do
  putStrLn $ "Computing package information for: " ++ pack pkgFullName
  s3req <- parseRequest s3url
  hackagereq <- parseRequest hackageurl
  mHashes <-
    httpSink
      s3req
      (\resS3 ->
         case statusCode $ responseStatus resS3 of
           200 -> Just <$> pairSink
           403 -> return Nothing
           _ ->
             throwM $
             StatusCodeException
               (responseStatus resS3)
               (responseHeaders resS3)
               (responseCookieJar resS3))
  hashesHackage <- httpSink hackagereq (const pairSink)
  mValidHashes <-
    case mHashes of
      Just hashes -> do
        if (hashes /= hashesHackage)
          then do
            hPutStrLn stderr $
              "Mismatched hashes between S3 and Hackage: " ++
              show (pkgFullName, hashes, hashesHackage)
            return Nothing
          else return $ Just hashes
      Nothing -> do
        putStrLn $ "Skipping file not yet on S3: " ++ pack pkgFullName
        return Nothing
  return $
    case mValidHashes of
      Nothing -> Nothing
      Just (hashes, size) ->
        Just
          Package
          { packageHashes = hashes
          , packageLocations = locations
          , packageSize = Identity size
          }
  where
    locations = [pack hackageurl, pack s3url]
    pkgFullName = getPackageFullName pkgName pkgVersion
    hackageurl =
      concat
        [hackageBaseUrl, "/package/", pkgFullName, "/", pkgFullName, ".tar.gz"]
    s3url = concat [mirrorFPComplete, "/package/", pkgFullName, ".tar.gz"]
    pairSink = getZipSink $ (,) <$> hashesSink <*> ZipSink lengthCE
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
