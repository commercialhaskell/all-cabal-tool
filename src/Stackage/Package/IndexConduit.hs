{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Stackage.Package.IndexConduit
  ( sourceTarFile
  , sourceAllCabalFiles
  , parseDistText
  , renderDistText
  , getCabalFilePath
  , getCabalFile
  , cabalFileConduit, sourceEntries
  , CabalFileEntry(..)
  ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, throwM)
import qualified Data.ByteString.Lazy as L
import Data.Conduit -- (Producer, bracketP, yield, (=$=))
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Version (Version)
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageName)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse
       (ParseResult, parsePackageDescription)
import Distribution.Text (disp, parse)
import qualified Distribution.Text
import System.FilePath ((</>), (<.>))
import System.IO (IOMode(ReadMode), hClose, openBinaryFile)
import Text.PrettyPrint (render)
import Debug.Trace

sourceTarFile
  :: MonadResource m
  => FilePath
  -> Producer m Tar.Entry
sourceTarFile fp = do
  bracketP (openBinaryFile fp ReadMode) hClose $
    \h -> do
      lbs <- liftIO $ L.hGetContents h
      loop $ Tar.read lbs
  where
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwM e
    loop (Tar.Next e es) = yield e >> loop es


--sourceTarball :: Source m S.ByteString -> Conduit S.ByteString m Tar.Entry
--sourceTarball tarball = do
--  entries <- Tar.read . L.fromChunks <$> lazyConsume tarball
--  sourceEntries entries
--  where
sourceEntries Tar.Done = return ()
sourceEntries (Tar.Next e rest) = yield e >> sourceEntries rest
sourceEntries (Tar.Fail e) = throwM e




data CabalFileEntry = CabalFileEntry
  { cfeName :: !PackageName
  , cfeVersion :: !Version
  , cfePath :: FilePath
  , cfeRaw :: L.ByteString
  , cfeParsed :: ParseResult GenericPackageDescription
  }


getCabalFilePath :: PackageName -> Version -> FilePath
getCabalFilePath (renderDistText -> pkgName) (renderDistText -> pkgVersion) =
  pkgName </> pkgVersion </> pkgName <.> "cabal"

  
getCabalFile :: PackageName -> Version -> L.ByteString -> CabalFileEntry
getCabalFile pkgName pkgVersion lbs =
  CabalFileEntry
  { cfeName = pkgName
  , cfeVersion = pkgVersion
  , cfePath = getCabalFilePath pkgName pkgVersion
  , cfeRaw = lbs
  , cfeParsed =
    parsePackageDescription $
    TL.unpack $ dropBOM $ decodeUtf8With lenientDecode lbs
  }
  -- https://github.com/haskell/hackage-server/issues/351
  where
    dropBOM t = fromMaybe t $ TL.stripPrefix (TL.pack "\xFEFF") t

sourceAllCabalFiles
  :: MonadResource m
  => FilePath -> Producer m CabalFileEntry
sourceAllCabalFiles indexTar = 
  sourceTarFile indexTar =$= CL.mapMaybe go
  where
    go e =
      case (toPkgVer $ Tar.entryPath e, Tar.entryContent e) of
        (Just (name', versions', name, version), Tar.NormalFile lbs _) ->
          Just (getCabalFile name version lbs ) { cfePath = Tar.entryPath e }
        _ -> Nothing
    toPkgVer s0 = do
      (name', '/':s1) <- Just $ break (== '/') s0
      (version', '/':s2) <- Just $ break (== '/') s1
      guard $ s2 == (name' ++ ".cabal")
      name <- parseDistText name'
      version <- parseDistText version'
      Just (name', version', name, version)


cabalFileConduit :: Monad m => Conduit Tar.Entry m CabalFileEntry
cabalFileConduit =
  CL.mapMaybe (\e -> getFileEntry e (toPkgVer $ Tar.entryPath e))
  where
    getFileEntry e@(Tar.entryContent -> Tar.NormalFile lbs _) (Just (pkgName, Just pkgVersion, fileName))
      | getCabalFilePath pkgName pkgVersion == Tar.entryPath e =
        Just $ getCabalFile pkgName pkgVersion lbs
      | otherwise = Nothing
    getFileEntry _ _ = Nothing
    toPkgVer s0 = do
      (pkgName', '/':s1) <- Just $ break (== '/') s0
      pkgName <- parseDistText pkgName'
      (mpkgVersion, fileName) <-
        case break (== '/') s1 of
          (fName, []) -> Just (Nothing, fName)
          (pkgVersion', '/':s2) -> do
            pkgVersion <- parseDistText pkgVersion'
            return $ (Just pkgVersion, s2)
      return (pkgName, mpkgVersion, fileName)

parseDistText
  :: (Monad m, Distribution.Text.Text t)
  => String -> m t
parseDistText s =
  case map fst $ filter (null . snd) $ readP_to_S parse s of
    [x] -> return x
    _ -> fail $ "Could not parse: " ++ s

renderDistText
  :: Distribution.Text.Text t
  => t -> String
renderDistText = render . disp
