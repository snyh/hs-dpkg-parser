{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Suite (
  Suite,
  loadSuite,

  SrcName,
  BinName,

  UrlFile,
  DSC,

  bdepends,
  rdepends,
  normalize,
  debootstrap,
  downloadDeb,

  build,

  oops,

  deb,
  url,

  listB,
  sourceSize,
  listV,

  deb2src,
  hasThePkg,
  hasTheSrc,
  getThePkg,
  getTheVirtual,
  getTheSrc,
) where

import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Debug.Trace
import           GHC.Generics         (Generic)
import           System.IO.Unsafe     (unsafePerformIO)

import qualified Data.ByteString.Lazy as B
import qualified Data.List            as L
import qualified Data.Map.Strict      as M

loadSuite :: [BinName] -> FilePath -> FilePath -> Suite
loadSuite base bfile sfile =
  let
    unsafeT f = unsafePerformIO $ loadObj M.empty f
    bs = unsafeT bfile
    suite = ((bs, initVirtuals bs), unsafeT sfile, M.empty)
  in
    foldr (\n acc -> markBuilded acc n SDone) suite base

loadObj :: (FromJSON a) => a -> FilePath -> IO a
loadObj e fpath =  let
  obj = do
    contents <- B.readFile fpath
    case eitherDecode contents of
      Left err -> trace err return Nothing
      Right v  -> return (Just v)
  in
  fromMaybe e <$> obj

type Suite = ((AllBinary, Virtuals), AllSource, AllStatus)


type AllBinary = M.Map String BinaryT
type Virtuals = M.Map BinName [BinName]
initVirtuals :: AllBinary -> Virtuals
initVirtuals = M.foldrWithKey (\k v virtuals -> recordVirtual k virtuals (allProvides v)) (M.empty::Virtuals) where
      recordVirtual real = foldr (\virtual -> M.insertWith (++) virtual [real])
      allProvides v = fromMaybe [] $ provides v


type AllSource = M.Map String SourceT
type AllStatus = M.Map BinName DebStatus

data DebStatus = SPending | SDone | SFailed deriving (Eq, Show)

listBy :: DebStatus -> Suite -> [BinName]
listBy s m = M.keys $ M.filter (==s) (debStatus m)

listV :: Suite -> Virtuals
listV ((_, v), _, _) = v

listB :: Suite -> AllBinary
listB ((b, _), _, _) = b

sourceSize :: Suite -> Int
sourceSize (_, s, _) = M.foldr ((+) . sizeOfSrc) 0 s where
  onlyDebian = filter (L.isInfixOf "debian" . url)
  sizeOfSrc = sum . map size . onlyDebian . files . getSrc
  getSrc = src :: SourceT -> DSC

oops :: Suite -> Int
oops (_, s, _) = sizeOf $ M.elems $ M.filter (not . hasDebianTarGz) s where
  hasDebianTarGz :: SourceT -> Bool
  hasDebianTarGz = any (L.isInfixOf "debian.tar" . url) . files . getSrc

  getSrc = src :: SourceT -> DSC

  sizeOf :: [SourceT] -> Int
  sizeOf = sum . map sizeOfSrc
  sizeOfSrc = sum . map size . files . getSrc

buildAll :: Suite -> Suite
buildAll s = foldSuiteB build s

foldSuiteB :: (Suite -> BinName -> Suite) -> Suite -> Suite
foldSuiteB fn s@((b, _), _, _) = foldl fn s (M.keys b)

hasTheSrc :: Suite -> SrcName -> Bool
hasTheSrc (_, s, _) n = M.member n s

debStatus :: Suite -> M.Map BinName DebStatus
debStatus (_, _, s) = s

getTheSrc :: Suite -> SrcName -> Maybe SourceT
getTheSrc (_, s, _) n = M.lookup n s

hasThePkg :: Suite -> BinName -> Bool
hasThePkg ((b, v), _, _) n = M.member n b || M.member n v

getThePkg :: Suite -> BinName -> Maybe BinaryT
getThePkg ((b, v), _ , _) n = M.lookup n b
getTheVirtual :: Suite -> BinName -> [BinaryT]
getTheVirtual ((b, v), _, _) n = catMaybes $ fromMaybe [] $ map (`M.lookup` b) <$> M.lookup n v


deleteThePkg :: Suite -> BinName -> Suite
deleteThePkg ((b,v), s, x) bname = ((M.delete bname b, v), s, x)

deb2src :: Suite -> BinName -> Maybe SrcName
deb2src s pkgname =
  let sname = (src :: BinaryT -> SrcName) <$> getThePkg s pkgname in
      hasTheSrc s <$> sname >> sname

_rdepends :: Suite -> BinName -> [BinName]
_rdepends s n =  if isNothing pkg then trace ("Panic with _rdepends " ++ n) [] else fromMaybe [] (runtimeDepends pkg') where
  pkg = getThePkg s n
  pkg' = fromJust pkg

_mergeBin :: Either String BinName -> Either String [BinName] -> Either String [BinName]
_mergeBin a b = (:) <$> a <*> b

normalize :: Suite -> [BinName] -> Either String [BinName]
normalize s pkgs = foldr (\i acc -> _mergeBin (fn i) acc) (Right []) pkgs where
  fn :: BinName -> Either String BinName
  fn n
    | not $ hasThePkg s n = Left ("The package " ++ n ++ " isn't existing in the suite")
    | isJust $ getThePkg s n = Right n
    | null $ getTheVirtual s n = Left ("Can't find virtual package of " ++ n)
    | otherwise = Right $ (name::BinaryT->String) $ head $ getTheVirtual s n

rdepends :: Suite -> BinName -> [BinName]
rdepends s pkgname = let
  s' = deleteThePkg s pkgname
  mydeps = _rdepends s pkgname
  subdeps = concatMap (_rdepends s') mydeps
  in
  L.sort $ L.nub $ mydeps ++ subdeps


hasBuilded :: Suite -> BinName -> Bool
hasBuilded (_, _, s) k = M.member k s

markBuilded :: Suite -> BinName -> DebStatus-> Suite
markBuilded (x, y, status) k v = (x, y, M.insert k v status)

markBuilded' :: Suite -> BinName -> DebStatus-> Suite
markBuilded' (x, y, status) k v = trace msg (x, y, M.insert k v status) where
  msg = case v of
    SFailed  -> "Can't build "
    SPending -> "Building "
    SDone    -> "Done "

build :: Suite -> BinName -> Suite
build s pkgname
  | hasBuilded s pkgname = s
  | not (hasThePkg s pkgname) = trace ("Unknown binary package " ++ pkgname) (markBuilded s pkgname SFailed)
  | isNothing $ deb2src s pkgname  = trace ("Unknown source package of " ++ pkgname) (markBuilded s pkgname SFailed)
  | otherwise = let
      bdeps = bdepends s pkgname
      deadloop = elem pkgname bdeps

      s' = foldl build s'' bdeps where
        s'' = markBuilded s pkgname SPending

      depsfail' = (not . null) $ listBy SFailed s'
    in
      markBuilded s' pkgname $ if deadloop || depsfail' then
                               SFailed
                             else
                               SDone

tryBuild :: Suite -> BinName -> [BinName]
tryBuild s n = listBy SFailed $ build s n

bdepends :: Suite -> BinName -> [BinName]
bdepends s pkgname = let
    mydeps = buildDepends <$> getTheSrc s pkgname
    xx = fromMaybe [] $ fromMaybe Nothing mydeps
  in
    foldr (\i acc -> rdepends s i ++ acc ) [] xx

type BinName = String
type SrcName = String
type PriorityName = String

data UrlFile = UrlFile {
  sha256 :: String
  ,size  :: Int
  ,url   :: String
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data BinaryT = BinaryT {
  name            :: String
  ,provides       :: Maybe [String]
  ,runtimeDepends :: Maybe [String]
  ,src            :: String
  ,priority       :: String
  ,deb            :: UrlFile
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DSC = DSC {
  name   :: String
  ,files :: [UrlFile]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SourceT = SourceT {
  name          :: String
  ,buildDepends :: Maybe [String]
  ,src          :: DSC
--  ,version       :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


nubJoin :: Eq a => [[a]] -> [a]
nubJoin = L.nub . join

debootstrap :: Suite -> [PriorityName] -> [BinName] -> [BinName]
debootstrap s@((bs,_), _, _) priorities base = let
  --baseDepends = nubJoin $ [rdepends s, bdepends s] <*> base
  baseDepends = nubJoin $ rdepends s <$> base
  required = M.foldr ( \v acc -> if priority v `elem` priorities then bname v:acc else acc) [] bs where
    bname = name :: BinaryT->String
  in
    baseDepends ++ required

downloadDeb :: Suite -> BinName -> Maybe UrlFile
downloadDeb s n = deb <$> getThePkg s n
