{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stackage.Package.Metadata.Types
  ( PackageInfo(..)
  , Deprecation(..)
  , CabalFile(..)
  , parseCabalFile
  ) where

import ClassyPrelude.Conduit hiding (pi)
import Data.Aeson
       (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL (stripPrefix)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Distribution.Package (PackageName, PackageIdentifier(PackageIdentifier))
import Distribution.PackageDescription
       (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse
       (ParseResult(..), parsePackageDescription)
import Distribution.Version (VersionRange)
import Stackage.Package.IndexConduit
       (parseDistText, renderDistText, getCabalFilePath)


data PackageInfo = PackageInfo
  { piLatest :: !Version
  , piHash :: !Text
  , piAllVersions :: !(Set Version)
  , piSynopsis :: !Text
  , piDescription :: !Text
  , piDescriptionType :: !Text
  , piChangeLog :: !Text
  , piChangeLogType :: !Text
  , piBasicDeps :: !(Map PackageName VersionRange)
  , piTestBenchDeps :: !(Map PackageName VersionRange)
  , piAuthor :: !Text
  , piMaintainer :: !Text
  , piHomepage :: !Text
  , piLicenseName :: !Text
  } deriving (Show, Eq, Typeable)

instance ToJSON PackageInfo where
  toJSON pi =
    object
      [ "latest" .= renderDistText (piLatest pi)
      , "hash" .= piHash pi
      , "all-versions" .= map renderDistText (Set.toList $ piAllVersions pi)
      , "synopsis" .= piSynopsis pi
      , "description" .= piDescription pi
      , "description-type" .= piDescriptionType pi
      , "changelog" .= piChangeLog pi
      , "changelog-type" .= piChangeLogType pi
      , "basic-deps" .= showM (piBasicDeps pi)
      , "test-bench-deps" .= showM (piTestBenchDeps pi)
      , "author" .= piAuthor pi
      , "maintainer" .= piMaintainer pi
      , "homepage" .= piHomepage pi
      , "license-name" .= piLicenseName pi
      ]
    where
      showM = Map.mapKeysWith const renderDistText . Map.map renderDistText

instance FromJSON PackageInfo where
  parseJSON =
    withObject "PackageInfo" $
    \o ->
       PackageInfo <$> (o .: "latest" >>= parseDistText) <*> o .: "hash" <*>
       (o .: "all-versions" >>= fmap Set.fromList . mapM parseDistText) <*>
       o .: "synopsis" <*>
       o .: "description" <*>
       o .: "description-type" <*>
       o .: "changelog" <*>
       o .: "changelog-type" <*>
       (o .: "basic-deps" >>= parseM) <*>
       (o .: "test-bench-deps" >>= parseM) <*>
       o .: "author" <*>
       o .: "maintainer" <*>
       o .: "homepage" <*>
       o .: "license-name"
    where
      parseM = fmap Map.fromList . mapM go . Map.toList
      go (name, range) = do
        name' <- parseDistText name
        range' <- parseDistText range
        return (name', range')

data Deprecation = Deprecation
  { depPackage :: !Text
  , depInFavourOf :: !(Set Text)
  }

instance ToJSON Deprecation where
  toJSON d =
    object
      ["deprecated-package" .= depPackage d, "in-favour-of" .= depInFavourOf d]

instance FromJSON Deprecation where
  parseJSON =
    withObject "Deprecation" $
    \o -> Deprecation <$> o .: "deprecated-package" <*> o .: "in-favour-of"


data CabalFile = CabalFile
  { cfRaw :: LByteString
  , cfPackageName :: !PackageName
  , cfPackageVersion :: !Version
  , cfPackageDescription :: !GenericPackageDescription
  }


parseCabalFile :: PackageName -> Version -> LByteString -> CabalFile
parseCabalFile pkgName pkgVersion lbs =
  CabalFile
  { cfRaw = lbs
  , cfPackageName = pkgName
  , cfPackageVersion = pkgVersion
  , cfPackageDescription = pkgDescription
  }
  where
    pkgDescription =
      case parseResult of
        ParseFailed perr ->
          error $
          "Stackage.Package.Metadata.Types.parseCabalFile: " ++
          "Error parsing cabal file <" ++
          getCabalFilePath pkgName pkgVersion ++ ">: " ++ show perr
        ParseOk _ gpd
          | package (packageDescription gpd) /= PackageIdentifier pkgName pkgVersion ->
            error $
            "Stackage.Package.Metadata.Types.parseCabalFile: " ++
            "parsed package identification mismatch: " ++
            show (PackageIdentifier pkgName pkgVersion)
        ParseOk _ gpd -> gpd
    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ TL.stripPrefix (pack "\xFEFF") t
    parseResult =
      parsePackageDescription $
      unpack $ dropBOM $ decodeUtf8With lenientDecode lbs
