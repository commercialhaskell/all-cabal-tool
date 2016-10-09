{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
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
       (HasHttpManager, HttpException(StatusCodeException), checkStatus,
        parseRequest, responseBody, responseCookieJar, responseHeaders,
        responseStatus, withResponse)
import Network.HTTP.Types (statusCode)
import System.Directory
import System.FilePath (dropExtension)

import Stackage.Package.Locations
import Stackage.Package.IndexConduit

type M env m = (HasHttpManager env, MonadThrow m, MonadBaseControl IO m, MonadReader env m, MonadIO m)

-- | Performs an update per package version:
--
-- * updates the .cabal file checks if hash file is present and matches the
-- * hashes in 'package.json'. Calculates it if is not present.
entryUpdateHashes
  :: M env m
  => GitRepository -> IndexFileEntry -> m ()
-- update cabal file with latest version and then compute package hashes if
-- missing
entryUpdateHashes hashesRepo (CabalFileEntry IndexFile {ifPackageVersion = Just pkgVersion
                                                       ,..}) = do
  liftIO $ repoFileWriter hashesRepo ifPath (`writeFile` ifRaw)
  -- TODO: Line below is likely unnecessary, since each cabal has a
  -- corresponding 'package.json' file, which is handled by the next case
  -- anyways.
  void $ createHashesIfMissing hashesRepo ifPackageName pkgVersion
-- Handle a possiblity of malformed 'package.json' file
entryUpdateHashes _ (HashesFileEntry IndexFile {ifParsed = Left err
                                               ,ifPath}) =
  liftIO $
  hPutStrLn stderr $
  "Stackage.Hackage.Hashes.entryUpdateHashes: There was an issue parsing: " ++
  ifPath ++ ". Parsing error: " ++ err
-- create hashes if not present yet and validate that values with Hackage do agree.
entryUpdateHashes hashesRepo (HashesFileEntry IndexFile {ifPackageVersion = Just pkgVersion
                                                        ,ifParsed = Right hackageHashes
                                                        ,..}) = do
  mpackage <- createHashesIfMissing hashesRepo ifPackageName pkgVersion
  case mpackage of
    Nothing -> return ()
    Just package -> mapM_ checkHash [tshow MD5, tshow SHA256]
      where checkHash hashType =
              unless
                (Map.lookup (toLower hashType) (hHashes hackageHashes) ==
                 Map.lookup hashType (packageHashes package))
                (error $
                 "Hash " ++
                 unpack hashType ++
                 "value mismatch for: '" ++
                 getPackageFullName ifPackageName pkgVersion ++
                 "' computed vs one from Hackage.")
  return ()
-- Write preferred versions file
entryUpdateHashes hashesRepo (PreferredVersionsEntry IndexFile {..}) = do
  liftIO $ repoFileWriter hashesRepo ifPath (`writeFile` ifRaw)
entryUpdateHashes _ _ = return ()

-- | If json file with package hashes is missing or corrupt (not parsable) it
-- downloads the taralls with source code and saves their the hashes.
createHashesIfMissing
  :: M env m
  => GitRepository -> PackageName -> Version -> m (Maybe (Package Identity))
createHashesIfMissing hashesRepo pkgName pkgVersion = do
  let jsonfp = dropExtension (getCabalFilePath pkgName pkgVersion) <.> "json"
  exists <- liftIO $ repoFileReader hashesRepo jsonfp doesFileExist
  mpackageHashes <-
    if exists
      then do
        res <- liftIO $ repoFileReader hashesRepo jsonfp readFile
        case eitherDecode' res of
          Left e -> error $ concat ["Could not parse ", jsonfp, ": ", e]
          Right x -> return $ flatten x
      else return Nothing
  case mpackageHashes of
    Just packageHashes -> return $ Just packageHashes
    Nothing -> do
      mpackageComputed <- computePackage pkgName pkgVersion
      case mpackageComputed of
        Nothing -> return Nothing
        Just packageHashes -> do
          liftIO $
            repoFileWriter hashesRepo jsonfp (`writeFile` encode packageHashes)
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
  :: M env m
  => PackageName -- ^ Package name
  -> Version -- ^ Package version
  -> m (Maybe (Package Identity))
computePackage pkgName pkgVersion = do
  putStrLn $ "Computing package information for: " ++ pack pkgFullName
  s3req <- parseRequest s3url
  hackagereq <- parseRequest hackageurl
  mhashes <-
    withResponse
      s3req
      { checkStatus = \_ _ _ -> Nothing
      } $
    \resS3 -> do
      case statusCode $ responseStatus resS3 of
        200 -> do
          hashesS3 <- responseBody resS3 $$ pairSink
          hashesHackage <-
            withResponse hackagereq $ \res -> responseBody res $$ pairSink
          when (hashesS3 /= hashesHackage) $
            error $
            "Mismatched hashes between S3 and Hackage: " ++
            show (pkgFullName, hashesS3, hashesHackage)
          return $ Just hashesS3
        403 -> do
          putStrLn $ "Skipping file not yet on S3: " ++ pack pkgFullName
          return Nothing
        _ ->
          throwM $
          StatusCodeException
            (responseStatus resS3)
            (responseHeaders resS3)
            (responseCookieJar resS3)
  let locations = [pack hackageurl, pack s3url]
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