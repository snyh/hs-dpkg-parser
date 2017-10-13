{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}


module Suite (
  shouldBuild,
  canBeBuild',
  deadPackages,

  whyCantBuildIt,

  dscHash,
  badSourceRecords,

  stage2,
  stage1,

  reduceDepend,

  findSourceBySrcName,
  priorities,

  listAllSources,
  listAllVirtuals,
  listAllBinaries,

  inspect,

  buildSuite,
  bin2srcName,

  M.empty
) where

import           Data.Either
import           Data.Foldable
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as SET
import           Debug.Trace
import qualified Record              as R
import           System.FilePath     (takeExtension)
import           Types
import           Utils               (fallback, hashArray)

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State as S
import           Data.List
import           Data.Monoid
import qualified Data.Text           as T

type Suite' = S.State Suite

dscHash :: DSC -> OutputHash
dscHash = T.pack . hashArray . map sha256 . files


toBinaries :: R.Record -> M.Map T.Text BinaryRecord
toBinaries r = M.fromList $ map build bins where
  bins :: [T.Text]
  bins = filter blackUdeb $ R.getArray "Binary" "," r where
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

toSourceRecord :: R.Record -> SourceRecord
toSourceRecord r = SourceRecord
  (R.get "Package" r)
  Nothing
  (case R.parseVersion $ R.get "Version" r of Left err -> error (show err) ; Right v -> v)
  (R.get "Architecture" r)
  (buildDSC (R.getMultilines "Checksums-Sha256" r))
  (buildDepends' (R.get "Build-Depends" r))
  (toBinaries r)

buildDepends' :: T.Text -> [Depend]
buildDepends' str = case R.parseDepends str of
                      Left err   -> trace (show err) []
                      Right deps -> deps

buildSuite :: BootstrapFunc -> Architecture -> SuitePrebuild -> [R.Record] -> Suite
buildSuite bfn arch c rs = s2 where
  s = Suite records cache arch M.empty
  s1 = S.execState (stage1 bfn >> stage2) s
  s2 = associatePrebuild c s1

  cache = (pkgs, listAllVirtuals s) where
    pkgs = foldr bFn M.empty (M.elems records) where
      bFn sr m = foldr (\bn m' -> M.insert bn v m') m bs where
        v = sname sr
        bs = M.keys $ outputs sr

  records = foldr' _insert M.empty rs
  _insert i s = let
    r = toSourceRecord i
    myname = sname r
    chooseNewer :: SourceRecord -> SourceRecord -> SourceRecord
    chooseNewer a b =
      let
        x = if a > b then a else b
        msg = "Discard old version record of " <> myname
      in
        x -- trace msg x
    in
    M.insertWith chooseNewer myname r s

extractVirtuals :: SourceRecord -> M.Map VirtualName [BinName]
extractVirtuals = foldr bFn M.empty . outputs where
  bFn :: BinaryRecord -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
  bFn b = unionWithKey thisMap where
    thisBName = bname b
    thisProvides = provides b
    thisMap = foldr (\x -> M.insert x [thisBName]) M.empty thisProvides

unionWithKey :: M.Map VirtualName [BinName] -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
unionWithKey = M.mergeWithKey (\_ v1 v2 -> Just (v1 <> v2)) id id

listAllVirtuals :: Suite -> M.Map BinName [BinName]
listAllVirtuals = foldr bFn M.empty . listAllSources where
  bFn :: SourceRecord -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
  bFn sr = unionWithKey vs' where
    vs' = extractVirtuals sr

listAllSources :: Suite -> [SourceRecord]
listAllSources = M.elems . suiteRecords

listAllBinaries :: Suite -> [BinaryRecord]
listAllBinaries = concatMap (M.elems . outputs) . M.elems . suiteRecords

putRecord :: SourceRecord -> Suite' ()
putRecord sr = do
  s <- S.get
  let rs = suiteRecords s
  S.put $ s { suiteRecords = M.insert (sname sr) sr rs }
  return ()

-- | findGoodDepends @s@ @sr@ 查找 @buildDepends sr@ 的@shash@必须不为@Nothing@
findGoodDepends :: Suite -> SourceRecord -> Either BinName [SourceRecord]
findGoodDepends s sr = ret where
  (badDeps :: [BinName], goodDeps) = partitionEithers $ map (reduceDepend s) (buildDepends sr)
  goodDeps' :: [Depend]
  goodDeps' = filter (/= IgnoreDepend) goodDeps

  badOne = find checker goodDeps'

  checker :: Depend -> Bool
  checker d = either (const True) (isNothing . shash) (dep2src d)

  ret
    | null badDeps = case badOne of
                       Nothing -> goodSrcs
                       Just d  -> Left $ dName d
    | otherwise = Left $ head badDeps

  goodSrcs :: Either T.Text [SourceRecord]
  goodSrcs = mapM dep2src goodDeps'

  dep2src :: Depend -> Either T.Text SourceRecord
  dep2src d = maybe (Left n) Right (findSourceByBinName s n) where
    n = dName d


-- | @reduceDepend s d@ 根据仓库@s@的状态，
--
-- 1. 若@d@为'Types.OneOfDepend', 则将@d@降级为'Types.SimpleDepend'.
-- 2. 若@d@为'Types.SimpleDepend',
--
--      - 若@s@满足@d@则返回@Just d@
--      - 若@dArchLimit@与@s@不匹配则直接忽略返回@Just IgnoreDepend@
--      - 否则返回@Nothing@
reduceDepend :: Suite -> Depend -> Either T.Text Depend
reduceDepend s d@SimpleDepend{ dName=n } =
  if | canIgnore -> Right IgnoreDepend
     | hasTheSource -> Right d
     | otherwise -> Left n where
  canIgnore = canSafeIgnoreDepend (suiteArch s) (ProfileName "nocheck") d
  hasTheSource = isJust $ bin2srcName s n
reduceDepend _ IgnoreDepend = Right IgnoreDepend
reduceDepend _ (OneOfDepend []) = Left "shouldn't go here"
reduceDepend s (OneOfDepend (d:ds)) = either (const tryNext) return (reduceDepend s d) where
  tryNext = reduceDepend s (OneOfDepend ds)

-- | @priorities s@ list the tuple of priority group and its counts
--
-- >>> priorities s
-- [(73,"required"),(82,"important"),(99,"standard"),(2568,""),(14265,"extra"),(40434,"optional")]
priorities :: Suite -> [(Int, T.Text)]
priorities s = sortOn fst $ fmap (length &&& head) $ (group . sort) $ map priority $ listAllBinaries s

-- | stage1 fn 根据fn的返回值来标记初始化时各SourceRecord的shash值
stage1 :: BootstrapFunc -> Suite' ()
stage1 fn = do
  s <- S.get
  let trySetSHash sr = case fn sr of
        Nothing -> sr
        Just sh -> sr { shash = Just sh }
      ss' = M.map trySetSHash (suiteRecords s)
  S.put s { suiteRecords = ss' }

-- | badSourceRecords @s@ 返回仓库@s@中无法编译的源码包名列表
badSourceRecords :: Suite -> [SrcName]
badSourceRecords = map sname . filter (isNothing . shash) . M.elems . suiteRecords

-- | deadPackages @s@ 返回仓库@s@源码中有依赖但无法从@s@中构建出来的二进制包名。
deadPackages :: Suite -> [T.Text]
deadPackages s = allBads where
  allDepends :: [Depend]
  allDepends = SET.toList $ foldr (\x acc -> SET.union acc $ SET.fromList (buildDepends x)) SET.empty (listAllSources s)
  allBads = lefts $ map (reduceDepend s) allDepends

-- | tryCalcSHash @sr@ 尝试计算@sr@的shash, 若成功则更新状态到@Suite'@中。返回成功与否。
tryCalcSHash :: SourceRecord -> Suite' Bool
tryCalcSHash sr = do
  s <- S.get

  let failMsg x = (error ("failed calc " ++ T.unpack (sname sr) ++ ":" ++ show x))

  let sethash h = putRecord (sr { shash = Just h }) >> return True

  let myhash = dscHash $ dsc sr :: OutputHash

  let depHashs = case findGoodDepends s sr of
        Left err -> trace (show err) $ Left err
        Right deps -> case mapM shash deps of
            Nothing -> error "HH"
            Just x  -> Right x

  let jh = T.pack . hashArray . (myhash:) <$> depHashs :: Either T.Text OutputHash

  either failMsg sethash jh

-- | stage2 根据stage1的结果，重复尝试计算shash
stage2 :: Suite' ([Int], Int)
stage2 = let
  -- | 生成所有零依赖的shash
  -- 记录新的状态，并返回本次成功处理的Record个数

  shouldBeBuild :: Suite -> [SourceRecord]
  shouldBeBuild s = filter v (listAllSources s) where
    v = either (const False) id . shouldBuild s

  loop' :: Suite' [Int]
  loop' = do
    s <- S.get
    oks <- mapM tryCalcSHash (shouldBeBuild s)
    if null oks then return [] else loop' >>= \xs -> return (length oks:xs)

  in do
  r <- loop'
  s <- S.get
  return (r, length $ badSourceRecords s)

findSourceByBinName :: Suite -> BinName -> Maybe SourceRecord
findSourceByBinName s n = bin2srcName s n >>= findSourceBySrcName s

findSourceBySrcName :: Suite -> SrcName -> Maybe SourceRecord
findSourceBySrcName s n = M.lookup n $ suiteRecords s

bin2srcName :: Suite -> BinName -> Maybe SrcName
bin2srcName s bn = result where
  (ss, vs) = suiteCache s
  debugR = if isNothing result then trace ("Can't find binary package " <> T.unpack bn) result else result

  result = if isJust d then d else v

  d :: Maybe SrcName
  d = M.lookup bn ss

  v :: Maybe SrcName
  v =  join (listToMaybe . mapMaybe (`M.lookup` ss) <$> M.lookup bn vs)


--canBeBuild' :: Suite -> SrcName -> Maybe Bool
canBeBuild' s sn = shouldBuild s $ fromJust $ findSourceBySrcName s sn

-- | @shouldBuild s sr@ 判断此'Types.SourceRecord'是否需要且可以直接计算shash
--
-- 1. @buildDepends sr@ 为空 && @shash sr == Nothing@  --> True
-- 2. forall e in BuildDepends of sr, the shash of e isn't Nothing -> True
-- 3. Otherwie -> False
shouldBuild :: Suite -> SourceRecord -> Either T.Text Bool
shouldBuild s sr = if
  | isJust $ shash sr -> Right False
  | null . buildDepends $ sr -> Right True
  | otherwise -> findGoodDepends s sr >> return True

binHashS :: Suite -> BinName -> Maybe OutputHash
binHashS s n = findSourceByBinName s n >>= (`binaryHash` n)

associatePrebuild :: SuitePrebuild -> Suite -> Suite
associatePrebuild c s = s { suitePrebuild = associatePrebuild' c s }

associatePrebuild' :: SuitePrebuild -> Suite -> M.Map OutputHash UrlFile
associatePrebuild' c s = M.fromList $ mapMaybe fn is where
  is :: [(BinName, UrlFile)]
  is = M.toList c

  fn :: (BinName, UrlFile) -> Maybe (OutputHash, UrlFile)
  fn = fn2 . first (binHashS s) where
    fn2 :: (Maybe OutputHash, UrlFile) -> Maybe (OutputHash, UrlFile)
    fn2 (Nothing, _) = Nothing
    fn2 (Just x, f)  = Just (x, f)

inspect :: Suite -> [(BinName, Maybe UrlFile)]
inspect s = h1 where
  bins = map bname $ listAllBinaries s

  h1 :: [(BinName, Maybe UrlFile)]
  h1 = map (wow s) bins

wow :: Suite -> BinName -> (BinName, Maybe UrlFile)
wow s n = (n, u) where
  u :: Maybe UrlFile
  u = do
    h <- binHashS s n
    M.lookup h (suitePrebuild s)

buildPackage :: Suite -> BinName -> IO ()
buildPackage s n = undefined where
  theSrc = findSourceByBinName s n
  depends = undefined


whyCantBuildIt :: Suite -> BinName -> T.Text
whyCantBuildIt s bn = why where
  Just sn = bin2srcName s bn
  Just sr = findSourceBySrcName s sn

  why = case shouldBuild s sr of
    Right _  -> "Success"
    Left bad -> trace ("Bad " ++ T.unpack bad) $ whyCantBuildIt s bad
