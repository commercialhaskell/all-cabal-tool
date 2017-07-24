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
import Crypto.Hash (hashlazy, SHA256(..))
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
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package
       (Dependency(..), PackageIdentifier(..), PackageName)
import Distribution.PackageDescription
       (CondTree(..), Condition(..), ConfVar(..),
        Flag(flagName, flagDefault), GenericPackageDescription, author,
        condBenchmarks, condExecutables, condLibrary, condTestSuites,
        description, genPackageFlags, homepage, license, maintainer,
        package, packageDescription, synopsis)
import Distribution.System (Arch(X86_64), OS(Linux))
import Distribution.Version
       (VersionRange, intersectVersionRanges, simplifyVersionRange,
        withinRange, Version, mkVersion)
import Distribution.Types.CondTree (CondBranch (..))
import Distribution.PackageDescription.Parse
       (ParseResult(..), parseGenericPackageDescription)
import Stackage.Package.IndexConduit
       (parseDistText, renderDistText)
import Stackage.Package.Hashes (unDigest)


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
       PackageInfo <$> (o .: "latest" >>= parseDistText) <*>
       o .: "hash" <*>
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


-- | Parsed @.cabal@ file. Only contains information needed for `PackageInfo`.
data CabalFile = CabalFile
  { cfPackage :: PackageIdentifier
  , cfHash :: !Text
  , cfSynopsis :: Text
  , cfBasicDeps :: Map PackageName VersionRange
  , cfTestBenchDeps :: Map PackageName VersionRange
  , cfAuthor :: Text
  , cfMaintainer :: Text
  , cfHomepage :: Text
  , cfLicenseName :: Text
  , cfDescription :: Text
  }


parseCabalFile :: FilePath -> LByteString -> CabalFile
parseCabalFile fp lbs =
  CabalFile
  { cfPackage = package pd
  , cfHash = unDigest SHA256 $ hashlazy lbs
  , cfSynopsis = pack $ synopsis pd
  , cfBasicDeps =
    combineDeps $
    maybe id ((:) . getDeps') (condLibrary gpd) $
    map (getDeps' . snd) (condExecutables gpd)
  , cfTestBenchDeps =
    combineDeps $
    map (getDeps' . snd) (condTestSuites gpd) ++
    map (getDeps' . snd) (condBenchmarks gpd)
  , cfAuthor = pack $ author pd
  , cfMaintainer = pack $ maintainer pd
  , cfHomepage = pack $ homepage pd
  , cfLicenseName = pack $ renderDistText $ license pd
  , cfDescription = pack $ description pd
  }
  where
    getDeps' = getDeps (getCheckCond gpd)
    pd = packageDescription gpd
    gpd =
      case parseResult of
        ParseFailed perr ->
          error $
          "Stackage.Package.Metadata.Types.parseCabalFile: " ++
          "Error parsing cabal file " ++ show fp ++ ": " ++ show perr
        ParseOk _ gpd' -> gpd'
    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ TL.stripPrefix (pack "\xFEFF") t
    parseResult =
      parseGenericPackageDescription $
      unpack $ dropBOM $ decodeUtf8With lenientDecode lbs


-- | FIXME these functions should get cleaned up and merged into stackage-common
getCheckCond :: GenericPackageDescription -> Condition ConfVar -> Bool
getCheckCond gpd = go
  where
    go (Var (OS os)) = os == Linux -- arbitrary
    go (Var (Arch arch)) = arch == X86_64 -- arbitrary
    go (Var (Flag flag)) = fromMaybe False $ Map.lookup flag flags -- arbitrary
    go (Var (Impl flavor range)) = flavor == GHC && ghcVersion `withinRange` range
    go (Lit b) = b
    go (CNot c) = not $ go c
    go (CAnd x y) = go x && go y
    go (COr x y) = go x || go y
    ghcVersion = mkVersion [8, 0, 2] -- arbitrary
    flags = Map.fromList $ map toPair $ genPackageFlags gpd
      where
        toPair f = (flagName f, flagDefault f)

getDeps
  :: (Condition ConfVar -> Bool)
  -> CondTree ConfVar [Dependency] a
  -> Map PackageName VersionRange
getDeps checkCond = goTree
  where
    goTree (CondNode _data deps comps) =
      combineDeps $
      map (\(Dependency name range) -> Map.singleton name range) deps ++
      map goComp comps
    goComp (CondBranch cond yes no)
      | checkCond cond = goTree yes
      | otherwise = maybe Map.empty goTree no

combineDeps :: [Map PackageName VersionRange] -> Map PackageName VersionRange
combineDeps =
  Map.unionsWith
    (\x y -> normalize . simplifyVersionRange $ intersectVersionRanges x y)
  where
    normalize vr =
      case parseDistText $ renderDistText vr of
        Nothing -> vr
        Just vr' -> vr'
