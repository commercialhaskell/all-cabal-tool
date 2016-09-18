{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import ClassyPrelude.Conduit
import Network.HTTP.Client (parseUrlThrow, responseCookieJar)
import Data.ByteArray.Encoding (convertToBase, Base (Base16))
import Network.HTTP.Simple
import Data.Function (fix)
import System.Environment (getEnv)
import Network.HTTP.Types (statusCode)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FilePath (dropExtension, takeDirectory, takeFileName)
import           Crypto.Hash.Conduit         (sinkHash)
import Data.Conduit.Process
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified System.IO as IO
import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              eitherDecode', encode, object,
                                              withObject, (.:), (.:?), (.=))
import           Crypto.Hash                 (HashAlgorithm, MD5 (..), Digest,
                                              SHA1 (..), SHA256 (..),
                                              SHA512 (..), Skein512_512 (..))

-- TODO need to add some kind of verification with hackage-security,
-- once I figure out how to use that package

indexTarGz :: FilePath
indexTarGz = "00-index.tar.gz"

rest :: Int
rest = 1000000 * 60 -- one minute

run :: FilePath -> FilePath -> [String] -> IO ()
run dir cmd args = do
    putStrLn $ concat
        [ "Running in "
        , tshow dir
        , ": "
        , unwords $ map pack $ cmd : args
        ]
    withCheckedProcessCleanup
        (proc cmd args) { cwd = Just dir }
        (\Inherited Inherited Inherited -> return ())

allCabalFiles, allCabalHashes, allCabalMetadata :: FilePath
allCabalFiles = "all-cabal-files"
allCabalHashes = "all-cabal-hashes"
allCabalMetadata = "all-cabal-metadata"

ensureGitRepo :: FilePath -- ^ Dest directory
              -> String -- ^ Branch
              -> IO ()
ensureGitRepo dir branch = do
    exists <- doesDirectoryExist dir
    if exists
        then do
            --run dir "git" ["clean", "-fdx"]
            run dir "git" ["remote", "set-url", "origin", url]
            run dir "git" ["fetch"]
            run dir "git" ["checkout", "origin/" ++ branch]
            run dir "git" ["branch", "-D", branch]
            run dir "git" ["checkout", "-b", branch]
            run dir "git" ["branch", "-u", "origin/" ++ branch]
            --run dir "git" ["clean", "-fdx"]
        else run "." "git"
            [ "clone"
            , "--depth=1"
            , url
            , dir
            , "--branch"
            , unpack branch
            ]
    run dir "git" ["config", "core.autocrlf", "input"]
  where
    url = "git@github.com:commercialhaskell/" ++ dir

main :: IO ()
main = do
    hackageRoot <- getEnv "HACKAGE_ROOT"
    indexReq <- parseRequest $ hackageRoot ++ "/packages/00-index.tar.gz"

    ensureGitRepo allCabalFiles "hackage"
    ensureGitRepo allCabalHashes "hackage"
    ensureGitRepo allCabalMetadata "master"

    let loop mlastEtag = do
            putStrLn $ "Checking index, etag == " ++ tshow mlastEtag
            let req = maybe id (addRequestHeader "if-none-match")
                      mlastEtag indexReq

                download = runResourceT $ httpSink req $ \res ->
                    case statusCode $ getResponseStatus res of
                        200 -> do
                            putStrLn "Downloading new index"
                            sinkFile indexTarGz
                            return ( True
                                   , listToMaybe $ getResponseHeader "etag" res
                                   )
                        304 -> return (False, mlastEtag)
                        _ -> error $ "Unexpected status: " ++
                                    show (getResponseStatus res)

            (downloaded, mnewEtag) <- runResourceT $ download `catchAny` \e -> do
                putStrLn $ "Exception occurred downloading index: " ++
                           tshow e
                return (False, mlastEtag)

            when downloaded $ doUpdate hackageRoot `catchAny` \e ->
                putStrLn $ "Exception on running update: " ++ tshow e

            threadDelay rest
            loop mnewEtag

    loop Nothing

sourceEntries :: (Exception e, MonadThrow m) => Tar.Entries e -> Producer m Tar.Entry
sourceEntries Tar.Done = return ()
sourceEntries (Tar.Next e rest) = yield e >> sourceEntries rest
sourceEntries (Tar.Fail e) = throwM e

toPkgVer :: String -> Maybe (Text, Text)
toPkgVer s@(stripSuffix ".cabal" . pack -> Just t0)
    | pkg == pkg2 = Just (pkg, ver)
    | otherwise = error $ "toPkgVer: could not parse " ++ s
  where
    (pkg, uncons -> Just ('/', t1)) = break (== '/') t0
    (ver, uncons -> Just ('/', pkg2)) = break (== '/') t1
toPkgVer _ = Nothing

doUpdate :: String -- ^ Hackage root
         -> IO ()
doUpdate hackageRoot = IO.withBinaryFile indexTarGz IO.ReadMode $ \indexH -> do
    lbsGZ <- hGetContents indexH
    let lbs = GZip.decompress lbsGZ
        entries = Tar.read lbs

    sourceEntries entries $$ mapM_C handleEntry
  where
    handleEntry entry
        | Just (pkg, ver) <- toPkgVer $ Tar.entryPath entry
        , Tar.NormalFile lbs _ <- Tar.entryContent entry = do
            exists <- liftIO $ doesFileExist jsonfp
            mpackage0 <- if exists
                then do
                    eres <- eitherDecode' <$> readFile jsonfp
                    case eres of
                        Left e -> error $ concat
                            [ "Could not parse "
                            , jsonfp
                            , ": "
                            , e
                            ]
                        Right x -> return $ flatten x
                else return Nothing
            mpackage <- case mpackage0 of
                Just package -> return $ Just package
                Nothing -> do
                    mpackage <- computePackage pkg ver
                    forM_ mpackage $ \package -> do
                        liftIO $ createDirectoryIfMissing True $ dropExtension jsonfp
                        writeFile jsonfp $ encode package
                    return mpackage
            case mpackage of
                Nothing -> return ()
                Just _ -> writeFile cabalfp lbs
            return ()
      where
        cabalfp = fromString $ Tar.entryPath entry
        jsonfp = dropExtension cabalfp <.> "json"
    handleEntry entry
        | takeFileName (Tar.entryPath entry) == "preferred-versions"
        , Tar.NormalFile lbs _ <- Tar.entryContent entry = do
            liftIO $ createDirectoryIfMissing True
                $ takeDirectory $ Tar.entryPath entry
            writeFile (Tar.entryPath entry) lbs
            return ()
    handleEntry _ = return ()

-- | Kinda like sequence, except not.
flatten :: Package Maybe -> Maybe (Package Identity)
flatten (Package h l ms) = Package h l . Identity <$> ms

computePackage :: Text -- ^ package
               -> Text -- ^ version
               -> IO (Maybe (Package Identity))
computePackage pkg ver = do
    putStrLn $ "Computing package information for: " ++ pack pkgver
    s3req <- parseRequest s3url
    hackagereq <- parseUrlThrow hackageurl

    mhashes <- httpSink s3req $ \resS3 -> do
        case statusCode $ getResponseStatus resS3 of
            200 -> do
                hashesS3 <- pairSink
                hashesHackage <- liftIO $ httpSink hackagereq $ const pairSink

                when (hashesS3 /= hashesHackage) $
                    error $ "Mismatched hashes between S3 and Hackage: " ++ show (pkg, ver, hashesS3, hashesHackage)

                return $ Just hashesS3
            403 -> do
                -- File not yet uploaded to S3
                putStrLn $ "Skipping file not yet on S3: " ++ pack pkgver
                return Nothing
            _ -> throwM $ StatusCodeException
                (getResponseStatus resS3)
                (getResponseHeaders resS3)
                (responseCookieJar resS3)

    let locations =
            [ pack hackageurl
            , pack s3url
            ]
    return $ case mhashes of
        Nothing -> Nothing
        Just (hashes, size) -> Just Package
            { packageHashes = hashes
            , packageLocations = locations
            , packageSize = Identity size
            }
  where
    pkgver = unpack pkg ++ '-' : unpack ver
    hackageurl = concat
        [ "https://hackage.haskell.org/package/"
        , pkgver
        , "/"
        , pkgver
        , ".tar.gz"
        ]
    s3url = concat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
        , pkgver
        , ".tar.gz"
        ]

    pairSink :: Monad m => Sink ByteString m (Map Text Text, Word64)
    pairSink = getZipSink $ (,) <$> hashesSink <*> ZipSink lengthCE

    hashesSink :: Monad m => ZipSink ByteString m (Map Text Text)
    hashesSink = fmap unions $ sequenceA
        [ mkSink SHA1
        , mkSink SHA256
        , mkSink SHA512
        , mkSink Skein512_512
        , mkSink MD5
        ]

mkSink :: (Monad m, Show hash, HashAlgorithm hash) => hash -> ZipSink ByteString m (Map Text Text)
mkSink ha = ZipSink $ do
    digest <- sinkHash
    return $ singletonMap (tshow ha) $ unDigest ha digest

unDigest :: hash -> Digest hash -> Text
unDigest _ = decodeUtf8 . convertToBase Base16

data Package f = Package
    { packageHashes    :: Map Text Text
    , packageLocations :: [Text] -- ^ why no ToJSON/FromJSON for Vector?
    , packageSize      :: f Word64
    }
instance ToJSON (Package Identity) where
    toJSON (Package h l (Identity s)) = object
        [ "package-hashes" .= h
        , "package-locations" .= l
        , "package-size" .= s
        ]
instance FromJSON (Package Maybe) where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .: "package-hashes"
        <*> o .: "package-locations"
        <*> o .:? "package-size"
