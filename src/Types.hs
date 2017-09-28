{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Types where

import           Data.Aeson
import qualified Data.Map     as M
import           Data.Monoid
import           Data.Ord
import           Data.Store
import qualified Data.Text    as T
import           GHC.Generics

type HashString = T.Text

-- | BinName names must consist only of lower case letters (a-z), digits (0-9), plus (+) and minus (-) signs, and periods (.). They must be at least two characters long and must start with an alphanumeric character.
type BinName = T.Text

-- | SrcName names must consist only of lower case letters (a-z), digits (0-9), plus (+) and minus (-) signs, and periods (.). They must be at least two characters long and must start with an alphanumeric character.
type SrcName = T.Text

type VirtualName = BinName

type DependsRecord = T.Text

data Version = Version {
  verEpoch     :: Integer
  ,verUpstream :: T.Text
  ,verRevision :: T.Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON, Store)

instance Ord Version where
  --TODO: To implement and verify logicals in https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version
  compare = comparing verEpoch <> comparing verUpstream <> comparing verRevision

data LimitVer =
  VerAny
  | VerGT Version
  | VerGTE Version
  | VerLT Version
  | VerLTE Version
  | VerEQ Version
  deriving (Eq, Show, Generic, FromJSON, ToJSON, Store)

type LimitArch = [Architecture]

data Architecture = ArchAny | ArchNative | ArchAll | ArchName T.Text | ArchIsNot Architecture
  deriving (Show, Generic, FromJSON, ToJSON, Store)

instance Eq Architecture where
  (==) (ArchIsNot a) b           = a /= b
  (==) a (ArchIsNot b)           = a /= b

  (==) ArchAny (ArchName _)      = True
  (==) (ArchName _) ArchAny      = True

  (==) ArchNative (ArchName _)   = True
  (==) (ArchName _) ArchNative   = True

  (==) (ArchName a) (ArchName b) = a == b

  (==) ArchAll ArchAll           = True
  (==) ArchAny ArchAny           = True
  (==) ArchNative ArchNative     = True

  (==) _ _                       = False

data Depend =
  OneOfDepend [Depend]
  | SimpleDepend {
      dName          :: BinName
      ,dVersionLimit :: LimitVer
      ,dArchLimit    :: LimitArch
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)


data BinaryRecord = BinaryRecord {
  bname         :: BinName
  ,depends      :: [Depend]
  ,provides     :: [BinName]
  ,priority     :: T.Text
  ,architecture :: T.Text
 } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data UrlFile = UrlFile {
  sha256 :: HashString
  ,size  :: Int
  ,url   :: T.Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data DSC = DSC {
  name   :: T.Text
  ,files :: [UrlFile]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data SourceRecord = SourceRecord {
  sname         :: SrcName
  ,shash        :: Maybe HashString
  ,version      :: Version
  ,architecture :: T.Text
  ,dsc          :: DSC
  ,buildDepends :: [Depend]
  ,outputs      :: M.Map T.Text BinaryRecord
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

instance Ord SourceRecord where
  compare = comparing sname <> comparing version

-- | isEssential sr 若sr所生成binaries里有任何一个package的priority
-- 属于 __required__或者__important__则为essentailas
isEssential :: SourceRecord -> Bool
isEssential sr = any _fpriority (M.elems $ outputs sr) where
    _fpriority = (`elem` ["required", "important"]) . priority

-- | Suite封装了一个deb仓库
data Suite = Suite {
  suiteRecords   :: SuiteRecords
  ,suiteCache    :: SuiteCache
  ,suiteArch     :: Architecture
  ,suitePrebuild :: SuitePrebuild
  } deriving (Show, Eq, Generic, Store)

type OptionName = T.Text
type OptionValue = T.Text

type SuiteCache = (M.Map BinName SrcName, M.Map VirtualName [BinName])


type BootstrapFunc = SourceRecord -> Maybe HashString

type SuiteRecords = M.Map SrcName SourceRecord
type SuitePrebuild = M.Map BinName UrlFile
