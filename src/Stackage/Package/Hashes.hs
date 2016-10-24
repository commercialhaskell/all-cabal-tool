{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Stackage.Package.Hashes where

import ClassyPrelude.Conduit
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import Crypto.Hash
       (HashAlgorithm, MD5(..), SHA1(..), SHA256(..), SHA512(..),
        Skein512_512(..), Digest)
import Crypto.Hash.Conduit (sinkHash)
import Data.Version (Version)
import Data.Aeson
       (FromJSON(..), ToJSON(..), eitherDecode', encode, object,
        withObject, (.:), (.:?), (.=))
import qualified Data.Map as Map
import Distribution.Package (PackageName)
import Network.HTTP.Client.Conduit
       (HttpException(StatusCodeException), parseRequest,
        responseCookieJar, responseHeaders, responseStatus)
import Network.HTTP.Types (statusCode)
import Network.HTTP.Simple (httpSink)
import System.FilePath (dropExtension)

import Stackage.Package.Git
import Stackage.Package.Locations
import Stackage.Package.IndexConduit

-- | Compares hashes in 'package.json' to the ones in the repo. In case a new
-- package.json appears without coresponding hashes in the repo, a package is
-- downloaded, hashes are computed and compared to ones in 'package.json' file.
-- 
entryUpdateHashes
  :: (MonadMask m, MonadIO m)
  => GitRepository -> IndexEntry -> m ()
entryUpdateHashes hashesRepo (PackageEntry IndexFile {ifFile = HackagePackage {..}
                                                     ,..}) = do
  mpackage <- createHashesIfMissing hashesRepo ifPackageName hackageVersion
  case mpackage of
    Nothing -> return ()
    Just package ->
      forM_ [tshow MD5, tshow SHA256] $
      \hashType ->
         unless
           (Map.lookup (toLower hashType) (hHashes hackageHashes) ==
            Map.lookup hashType (packageHashes package))
           (error $
            "Stackage.Hackage.Hashes.entryUpdateHashes: Hash " ++
            unpack hashType ++
            "value mismatch for: '" ++
            getPackageFullName ifPackageName hackageVersion ++
            "' computed vs one from Hackage.")
entryUpdateHashes _ _ = return ()

-- | If json file with package hashes is missing or corrupt (not parsable) it
-- downloads the taralls with source code and saves their the hashes.
createHashesIfMissing
  :: (MonadMask m, MonadIO m)
  => GitRepository -> PackageName -> Version -> m (Maybe (Package Identity))
createHashesIfMissing hashesRepo pkgName pkgVersion =
  liftIO $
  do let jsonfp = dropExtension (getCabalFilePath pkgName pkgVersion) <.> "json"
     meres <- (fmap eitherDecode') <$> repoReadFile hashesRepo jsonfp
     let mpackageHashes =
           case meres of
             (Just (Left e)) ->
               error $ concat ["Could not parse ", jsonfp, ": ", e]
             (Just (Right x)) -> flatten x
             _ -> Nothing
     case mpackageHashes of
       Just packageHashes -> return $ Just packageHashes
       Nothing -> do
         mpackageComputed <- computePackage pkgName pkgVersion
         case mpackageComputed of
           Nothing -> return Nothing
           Just packageHashes -> do
             repoWriteFile hashesRepo jsonfp (encode packageHashes)
             return $ Just packageHashes

-- | Kinda like sequence, except not.
flatten :: Package Maybe -> Maybe (Package Identity)
flatten (Package h l ms) = Package h l . Identity <$> ms

data Package f = Package
  { packageHashes :: Map Text Text
  , packageLocations :: [Text]
  , packageSize :: f Word64
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
  mhashes <-
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
  case mhashes of
    Just hashes ->
      when (hashes /= hashesHackage) $
      error $
      "Mismatched hashes between S3 and Hackage: " ++
      show (pkgFullName, hashes, hashesHackage)
    Nothing -> putStrLn $ "Skipping file not yet on S3: " ++ pack pkgFullName
  return $
    case mhashes of
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
