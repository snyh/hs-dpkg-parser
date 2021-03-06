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
import           Utils        (hashArray, toInt)

type OutputHash = T.Text

-- | BinName names must consist only of lower case letters (a-z), digits (0-9), plus (+) and minus (-) signs, and periods (.). They must be at least two characters long and must start with an alphanumeric character.
type BinName = T.Text

-- | SrcName names must consist only of lower case letters (a-z), digits (0-9), plus (+) and minus (-) signs, and periods (.). They must be at least two characters long and must start with an alphanumeric character.
type SrcName = T.Text

type VirtualName = BinName

data Version = Version {
  verEpoch     :: Integer
  ,verUpstream :: T.Text
  ,verRevision :: T.Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON, Store)

instance Ord Version where
  --TODO: To implement and verify logicals in https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version
  compare = comparing verEpoch <> comparing (toIntArray . verUpstream) <> comparing (toIntArray . verRevision) where
    toIntArray = map toInt . T.split (`elem` ['.', '~', '+'])

data LimitVer =
  VerAny
  | VerLT Version
  | VerLTE Version
  | VerEQ Version
  | VerGTE Version
  | VerGT Version
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Store)

type LimitArch = [Architecture]
type LimitProfile = [Profile]

data Profile =
  ProfileNone
  | ProfileName T.Text
  | ProfileIsNot Profile
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Store)

data Architecture =
  ArchAny
  | ArchNative
  | ArchAll
  -- | @ArchName "amd64"@
  | ArchName T.Text
  | ArchIsNot Architecture
  deriving (Show, Ord, Generic, FromJSON, ToJSON, Store)

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
  -- | 可以安全的被忽略
  IgnoreDepend

  -- | 满足任意一个Depend即可
  | OneOfDepend [Depend]

  | SimpleDepend {
      dName          :: BinName
      ,dVersionLimit :: LimitVer
      ,dArchLimit    :: LimitArch
      ,dProfile      :: LimitProfile
      }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Store)

canSafeIgnoreDependByArch :: Architecture -> Depend -> Bool
canSafeIgnoreDependByArch arch SimpleDepend{dArchLimit=limits} = notEmpty && (inNotList || not inList) where
  notEmpty = not (null limits)
  inNotList = arch `elem` notList
  inList = arch `elem` list

  isNot :: Architecture -> Bool
  isNot (ArchIsNot _) = True
  isNot _             = False
  fromNot :: Architecture -> Architecture
  fromNot (ArchIsNot x) = x
  fromNot x             = x

  notList = map fromNot $ filter isNot limits
  list = filter (not . isNot) limits
canSafeIgnoreDependByArch _ _ = error "it can only used with SimpleDepend"

canSafeIgnoreDependByProfile :: Profile -> Depend -> Bool
canSafeIgnoreDependByProfile p SimpleDepend{dProfile=limits} = notEmpty && canIgnore where
  notEmpty = not (null limits)
  canIgnore
    | p == ProfileNone = null notList
    | otherwise = inNotList || not inList

  inNotList = p `elem` notList

  inList = p `elem` list

  isNot :: Profile -> Bool
  isNot (ProfileIsNot (ProfileName _)) = True
  isNot _                              = False
  fromNot :: Profile -> Profile
  fromNot (ProfileIsNot x) = x
  fromNot x                = x

  notList = map fromNot $ filter isNot limits
  list = filter (not . isNot) limits
canSafeIgnoreDependByProfile _ _ = error "it can only used with SimpleDepend"

canSafeIgnoreDepend :: Architecture -> Profile -> Depend -> Bool
canSafeIgnoreDepend a p d = canSafeIgnoreDependByProfile p d || canSafeIgnoreDependByArch a d


data BinaryRecord = BinaryRecord {
  bname         :: BinName
  ,depends      :: [Depend]
  ,provides     :: [BinName]
  ,priority     :: T.Text
  ,architecture :: T.Text
 } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

-- | @binaryHash s n@ calculate the hash value of binary record @n@ in the @s@
binaryHash :: SourceRecord -> BinName -> Maybe OutputHash
binaryHash s n = do
  sh <- shash s
  bin <- M.lookup n (outputs s)
  return $ T.pack $ hashArray [sh, bname bin]

data UrlFile = UrlFile {
  sha256 :: T.Text
  ,size  :: Int
  ,url   :: T.Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data DSC = DSC {
  name   :: T.Text
  ,files :: [UrlFile]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, Store)

data SourceRecord = SourceRecord {
  sname         :: SrcName
  ,shash        :: Maybe OutputHash
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
    _fpriority = (`elem` ["required", "important", "standard"]) . priority

-- | Suite封装了一个deb仓库
data Suite = Suite {
  suiteRecords   :: SuiteRecords
  ,suiteCache    :: SuiteBinNameCache
  ,suiteArch     :: Architecture
  ,suitePrebuild :: SuitePrebuild
  } deriving (Show, Generic, Store)

type SuiteBinNameCache = (M.Map BinName SrcName, M.Map VirtualName [BinName])

type BootstrapFunc = SourceRecord -> Maybe OutputHash

type SuiteRecords = M.Map SrcName SourceRecord
type SuitePrebuild = M.Map BinName UrlFile
