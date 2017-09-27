{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Types  where

import           Data.Aeson
import qualified Data.Map     as M
import           Data.Monoid
import           Data.Ord
import           Data.Store
import           Data.Text    as T
import           GHC.Generics

type HashString = Text
type BinName = Text
type SrcName = Text
type VirtualName = BinName

type DependsRecord = Text

data Version = Version {
  verEpoch     :: Integer
  ,verUpstream :: Text
  ,verRevision :: Text
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

data Architecture = ArchAny | ArchNative | ArchAll | ArchName Text | ArchIsNot Architecture
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
  | Depend {
      dName          :: BinName
      ,dVersionLimit :: LimitVer
      ,dArchLimit    :: LimitArch
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)


data BinaryRecord = BinaryRecord {
  bname         :: BinName
  ,depends      :: [Depend]
  ,provides     :: [BinName]
  ,priority     :: Text
  ,architecture :: Text
 } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data UrlFile = UrlFile {
  sha256 :: HashString
  ,size  :: Int
  ,url   :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data DSC = DSC {
  name   :: Text
  ,files :: [UrlFile]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data SourceRecord = SourceRecord {
  sname         :: SrcName
  ,shash        :: Maybe HashString
  ,version      :: Version
  ,architecture :: Text
  ,dsc          :: DSC
  ,buildDepends :: [Depend]
  ,outputs      :: M.Map Text BinaryRecord
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

instance Ord SourceRecord where
  compare = comparing sname <> comparing version


data Suite = Suite {
  suiteRecords   :: SuiteRecords
  ,suiteCache    :: SuiteCache
  ,suiteProfile  :: SuiteProfile
  ,suitePrebuild :: SuitePrebuild
  } deriving (Show, Eq, Generic, Store)

type OptionName = T.Text
type OptionValue = T.Text
type SuiteCache = (M.Map BinName SrcName, M.Map VirtualName [BinName])
type SuiteProfile = M.Map OptionName OptionValue
type SuiteRecords = M.Map SrcName SourceRecord
type SuitePrebuild = M.Map BinName UrlFile
