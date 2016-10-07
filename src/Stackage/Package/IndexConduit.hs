{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Stackage.Package.IndexConduit
  ( sourceTarFile
  , sourceAllCabalFiles
  , parseDistText
  , renderDistText
  , getCabalFilePath
  , getCabalFile
  , CabalFileEntry(..)
  ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, throwM)
import qualified Data.ByteString.Lazy as L
import Data.Conduit (Producer, bracketP, yield, (=$=))
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
sourceAllCabalFiles indexTar = do
  sourceTarFile indexTar =$= CL.mapMaybe go
  where
    go e =
      case (toPkgVer $ Tar.entryPath e, Tar.entryContent e) of
        (Just (name, version), Tar.NormalFile lbs _) ->
          Just $ getCabalFile name version lbs
        _ -> Nothing
    toPkgVer s0 = do
      (name', '/':s1) <- Just $ break (== '/') s0
      (version', '/':s2) <- Just $ break (== '/') s1
      guard $ s2 == (name' ++ ".cabal")
      name <- parseDistText name'
      version <- parseDistText version'
      guard $ s0 == getCabalFilePath name version
      Just (name, version)

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
