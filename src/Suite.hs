{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}


module Suite (
  SrcName,
  BinName,
  UrlFile,
  BinaryRecord,
  SourceRecord,
  DSC,
  Suite,
  SuiteProfile,

  binOutput,

  loadSuite,
  findSource,

  priorities,

  virtuals,
  buildVirtuals,
  buildBinaries,
  buildSourceRecord,
  dep2src,
  findDepends,

  buildSuite,
  bin2src,
) where

import           Data.Either
import           Data.Foldable
import qualified Data.Map            as M
import           Data.Maybe
import           Debug.Trace
import qualified Record              as R
import           System.FilePath     (takeExtension)
import           Types
import           Utils               (fallback, hashArray, loadObject)

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State as S
import           Data.List
import           Data.Monoid
import qualified Data.Text           as T

type OptionName = T.Text
type OptionValue = T.Text

type Suite = (SuiteRecords, SuiteCache, SuiteProfile)
type SuiteCache = (M.Map BinName SrcName, M.Map VirtualName [BinName])
type SuiteProfile = M.Map OptionName OptionValue
type SuiteRecords = M.Map SrcName SourceRecord
type Suite' = S.State Suite

buildBinaries :: R.Record -> M.Map T.Text BinaryRecord
buildBinaries r = M.fromList $ map build allBinaries where
  allBinaries :: [T.Text]
  allBinaries = filter blackUdeb $ R.getArray "Binary" "," r where
    blackUdeb bn = t == "deb" where
      (t, _ ,_) = M.findWithDefault ("deb", "", "") bn allOthers

  allProvides = foldr (\(n, ps) acc -> M.insert n ps acc) M.empty xs where
    xs = rights $ map R.parseBinProvides (R.getMultilines "BinProvides" r)

  allDepends = foldr (\(n,deps) acc -> M.insert n deps acc) M.empty xs where
    xs = rights $ map R.parseBinDepends (R.getMultilines "BinDepends" r)

  allOthers = foldr (\(n,t,p,a) -> M.insert n (t,p,a)) M.empty xs where
    xs = filter (\(_,t,_,_) -> t == T.pack "deb") $ rights $ map R.parsePkgList (R.getMultilines "Package-List" r)

  defaultPriority :: T.Text
  defaultPriority = R.get "Priority" r
  defaultArch :: T.Text
  defaultArch = R.get "Architecture" r

  build bn = (bn, BinaryRecord bn deps provs pri arch) where
    pri = fallback priority' defaultPriority
    arch = fallback arch' defaultArch
    (_, priority', arch') = M.findWithDefault (T.pack "deb", T.pack "", T.pack"") bn allOthers

    provs = M.findWithDefault [] bn allProvides
    deps = M.findWithDefault [] bn allDepends

buildDSC :: [T.Text] -> DSC
buildDSC xs = DSC dscName fs where
  fs :: [UrlFile]
  fs = rights $ map R.parseUrl xs
  dscName = foldr (\i r -> let p = url i in if takeExtension (T.unpack p) == ".dsc" then p else r) "unknown" fs

buildSourceRecord :: R.Record -> SourceRecord
buildSourceRecord r = SourceRecord
  (R.get "Package" r)
  Nothing
  (case R.parseVersion $ R.get "Version" r of Left err -> error (show err) ; Right v -> v)
  (R.get "Architecture" r)
  (buildDSC (R.getMultilines "Checksums-Sha256" r))
  (buildDepends' (R.get "Build-Depends" r))
  (buildBinaries r)

buildDepends' :: T.Text -> [Depend]
buildDepends' str = case R.parseDepends str of
                      Left err   -> trace (show err) []
                      Right deps -> deps

buildSuite :: SuiteProfile -> [R.Record] -> Suite
buildSuite profile rs = (records, cache, profile) where
  cache = (pkgs, virtuals records) where
    pkgs = foldr bFn M.empty (M.elems records) where
      bFn sr m = foldr (\bn m' -> M.insert bn v m') m bs where
        v = sname sr
        bs = M.keys $ outputs sr

  records = foldr' _insert M.empty rs
  _insert i s = let
    r = buildSourceRecord i
    myname = sname r
    chooseNewer :: SourceRecord -> SourceRecord -> SourceRecord
    chooseNewer a b =
      let
        x = if a > b then a else b
        msg = "Discard old version record of " ++ T.unpack myname
      in
        x -- trace msg x
    in
    M.insertWith chooseNewer myname r s

buildVirtuals :: SourceRecord -> M.Map VirtualName [BinName]
buildVirtuals = foldr bFn M.empty . outputs where
  bFn :: BinaryRecord -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
  bFn b = unionWithKey thisMap where
    thisBName = bname b
    thisProvides = provides b
    thisMap = foldr (\x -> M.insert x [thisBName]) M.empty thisProvides

unionWithKey :: M.Map VirtualName [BinName] -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
unionWithKey = M.mergeWithKey (\_ v1 v2 -> Just (v1 <> v2)) id id

virtuals :: SuiteRecords -> M.Map BinName [BinName]
virtuals = foldr bFn M.empty . M.elems where
   bFn :: SourceRecord -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
   bFn sr = unionWithKey vs' where
     vs' = buildVirtuals sr

binOutput :: SourceRecord -> BinName -> HashString
binOutput sr bn = fromMaybe "PendingHash" (shash sr)

badSourceRecords :: SuiteRecords -> [SourceRecord]
badSourceRecords = filter (isNothing . shash) . M.elems

dscHash :: DSC -> HashString
dscHash d = T.pack $ hashArray $ map sha256 $ files d

getRecord :: SrcName -> Suite' (Maybe SourceRecord)
getRecord sn = do
  (s, _, _) <- S.get
  return $ M.lookup sn s

putRecord :: SourceRecord -> Suite' ()
putRecord sr = do
  (s, c, p) <- S.get
  S.put (M.insert (sname sr) sr s, c, p)
  return ()

filterBuildDepends :: Suite -> SourceRecord -> [Depend]
filterBuildDepends (_, _, profile) sr = filterDeps parch deps where
  deps = buildDepends sr

  filterDeps :: Maybe Architecture -> [Depend] -> [Depend]
  filterDeps _ [] = []
  filterDeps Nothing xs         = xs
  filterDeps (Just arch) (x:xs) = if p arch x then x: rem' else rem' where
    rem' = filterDeps (Just arch) xs
    p :: Architecture -> Depend -> Bool
    p a (OneOfDepend xs')   = any (p a) xs'
    p a (Depend _ _ aLimit) = a `elem` aLimit

  parch :: Maybe Architecture
  parch = M.lookup "Arch" profile >>= \i ->  case R.parseOnly R.pArchitecture i of
      Left _  -> Nothing
      Right x -> return x


findSource :: Suite -> SrcName ->  Maybe SourceRecord
findSource (rs, _, _) n = M.lookup n rs

findDepends :: Suite -> SourceRecord -> Either [BinName] [SourceRecord]
findDepends s sr = deps where
    deps :: Either [BinName] [SourceRecord]
    deps = if null failed then Right success else Left failed  where
      failed :: [BinName]
      failed = map fst $ filter (isNothing . snd) cache

      success :: [SourceRecord]
      success = map (fromJust . snd) $ filter (isJust . snd) cache

      cache :: [(BinName, Maybe SourceRecord)]
      cache = map (depName &&& (dep2src s >=> findSource s)) (filterBuildDepends s sr)

    depName :: Depend -> BinName
    depName (OneOfDepend []) = ""
    depName (OneOfDepend (x:xs)) = depName x <> " or " <> depName (OneOfDepend xs)
    depName d = dName d

priorities :: Suite -> [(Int, T.Text)]
priorities (s, _, _) = sortOn fst $ fmap (length &&& head) $ (group . sort) $ map priority (allBins $ M.elems s) where
  allBins = concatMap (M.elems . outputs)

dep2src :: Suite -> Depend -> Maybe SrcName
dep2src _ (OneOfDepend []) = Nothing
dep2src s (OneOfDepend (x:xs)) = let t = dep2src s x in if isJust t then t else dep2src s (OneOfDepend xs)
dep2src s dep                  = bin2src s $ dName dep

calcByDSC :: SourceRecord -> Suite' ()
calcByDSC sr = putRecord (sr { shash = Just $ dscHash $ dsc sr })

-- stage1 直接根据dsc来设置base pkgs的shash
stage1 :: Suite' ()
stage1 = S.get >>= mapM_ calcByDSC . _essentials where
  _essentials :: Suite -> [SourceRecord]
  _essentials (s, _, _) = filter fn $ M.elems s where
    fn :: SourceRecord -> Bool
    fn sr = any _fpriority (M.elems $ outputs sr)
    _fpriority = (`elem` ["required", "important"]) . priority

-- stage2 根据stage1的结果，重复尝试计算shash
stage2 :: Suite' ([Int], Int)
stage2 = do
  r <- hardwork
  (s, _, _) <- S.get
  return (r, length $ badSourceRecords s)
  where
    hardwork :: Suite' [Int]
    hardwork = do
      x <- calc
      if x == 0 then
        return []
        else
        do
          xs <- hardwork
          return (x:xs)

loadSuite :: FilePath -> IO Suite
loadSuite fp = S.execState (stage1 >> stage2) <$> loadObject fp


bin2src :: Suite -> BinName -> Maybe SrcName
bin2src (s, (xx, vs), _) bn = debugR where
  debugR = if isNothing result then trace ("Can't find " <> T.unpack bn) result else result

  result = if isJust d then d else v

  d :: Maybe SrcName
  d = M.lookup bn xx

  v :: Maybe SrcName
  v =  join (listToMaybe . mapMaybe (`M.lookup` xx) <$> M.lookup bn vs)


-- canBeCalc 判断此Record是否可以被直接计算出shash
-- 1. BuildDepends 为空 && shash == Nothing --> True
-- 2. forall e in BuildDepends, the shash of e isn't Nothing -> True
-- 3. Otherwie -> False
canBeCalc :: Suite -> SourceRecord -> Bool
canBeCalc s sr = if
  | isJust $ shash sr -> False
  | null . buildDepends $ sr -> True
  | otherwise -> case v of
      Right r -> r
      Left xs -> trace (T.unpack myname ++ " Failed " ++ show xs) False
  where
    myname = sname sr
    v = p2 sr
    p2 sr' = all (isJust . shash) <$> findDepends s sr'

-- 生成所有零依赖的shash
calc :: Suite' Int
calc = do
  s@(sr, _, _) <- S.get
  let ss = filter (canBeCalc s) $ M.elems sr
  oks <- mapM tryCalc ss
  return $ length oks

tryCalc :: SourceRecord -> Suite' Bool
tryCalc sr = do
  s <- S.get

  let failMsg x = (error ("failed calc " ++ T.unpack (sname sr) ++ ":" ++ show x))

  let sethash h = putRecord (sr { shash = Just h }) >> return True

  let myhash = dscHash $ dsc sr :: HashString

--  let depHashs = findDepends s sr >>= mapM shash :: Either [String] [HashString]
  let depHashs = case findDepends s sr of
        Left err -> trace (show err) $ Left err
        Right deps -> case mapM shash deps of
            Nothing -> Left ["HH"]
            Just x  -> Right x

  let jh = T.pack . hashArray . (myhash:) <$> depHashs :: Either [T.Text] HashString

  either failMsg sethash jh
