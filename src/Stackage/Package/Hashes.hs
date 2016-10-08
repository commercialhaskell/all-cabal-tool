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
import qualified Codec.Archive.Tar as Tar
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import Crypto.Hash
       (HashAlgorithm, MD5(..), SHA1(..), SHA256(..), SHA512(..),
        Skein512_512(..), Digest)
import Crypto.Hash.Conduit (sinkHash)
import Data.Version (Version)
import Data.Aeson
       (FromJSON(..), ToJSON(..), eitherDecode', encode, object,
        withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Base16 as B16
import Data.Conduit.Lazy (lazyConsume)
import Data.Conduit.Zlib (ungzip)
import Distribution.Package (PackageName)
import qualified Data.Text.Lazy.Builder.Int
import Network.HTTP.Client.Conduit
       (HasHttpManager, HttpException(StatusCodeException), checkStatus,
        parseUrl, responseBody, responseCookieJar, responseHeaders,
        responseStatus, withManager, withResponse)
import Network.HTTP.Types (statusCode)
import System.Directory
import System.FilePath (dropExtension, takeDirectory, takeFileName)

import Stackage.Package.Locations
import Stackage.Package.IndexConduit


type M env m = (HasHttpManager env,
                MonadThrow m,
                MonadBaseControl IO m,
                MonadReader env m,
                MonadIO m)



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



handleEntry
  :: M env m
  => GitRepository -> IndexFileEntry -> m ()
-- update cabal file with latest version and then compute package hashes if
-- missing
handleEntry hashesRepo (CabalFileEntry IndexFile {ifPackageVersion = Just pkgVersion
                                                 ,..}) = do
  liftIO $ repoFileWriter hashesRepo ifPath (`writeFile` ifRaw)
  void $ createHashesIfMissing hashesRepo ifPackageName pkgVersion
-- Handle a possiblity of malformed 'package.json' file
handleEntry hashesRepo (HashesFileEntry IndexFile {ifParsed = Left err, ifPath}) = return ()
  --liftIO $
  --  hPutStrLn stderr $ "AllCabalHashes: There was an issue parsing: " ++ ifPath
-- create hashes if not present yet and validate that values do agree.
handleEntry hashesRepo (HashesFileEntry IndexFile {ifPackageVersion = Just pkgVersion
                                                  ,ifParsed = Right hashes, ..}) = do
  packageHashes <- createHashesIfMissing hashesRepo ifPackageName pkgVersion
  -- TODO: validate that computed hashes match ones from hackage.
  return ()
-- Write preferred versions file
handleEntry hashesRepo (PreferredVersionsEntry IndexFile {..}) = do
  liftIO $ repoFileWriter hashesRepo ifPath (`writeFile` ifRaw)
handleEntry _ _ = return ()
  
{-
handleEntry entry
  | takeFileName (Tar.entryPath entry) == "preferred-versions"
  , Tar.NormalFile lbs _ <- Tar.entryContent entry = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory $ Tar.entryPath entry
    writeFile (Tar.entryPath entry) lbs
    return 0
handleEntry _ = return 0
-}

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
  => PackageName -- ^ package
  -> Version -- ^ version
  -> m (Maybe (Package Identity))
computePackage (renderDistText -> pkg) (renderDistText -> ver) = do
  putStrLn $ "Computing package information for: " ++ pack pkgver
  s3req <- parseUrl s3url
  hackagereq <- parseUrl hackageurl
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
            show (pkg, ver, hashesS3, hashesHackage)
          return $ Just hashesS3
        403
        -- File not yet uploaded to S3
         -> do
          putStrLn $ "Skipping file not yet on S3: " ++ pack pkgver
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
    pkgver = pkg ++ '-' : ver
    hackageurl =
      concat
        ["https://hackage.haskell.org/package/", pkgver, "/", pkgver, ".tar.gz"]
    s3url =
      concat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
        , pkgver
        , ".tar.gz"
        ]
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
