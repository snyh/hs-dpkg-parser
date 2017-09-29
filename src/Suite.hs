{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}


module Suite (
  canBeBuild,
  dscHash,
  badSourceRecords,

  reduceDepend,

  findSourceBySrcName,
  priorities,

  inspect,

  findDepends,

  buildSuite,
  bin2srcName,
) where

import           Data.Either
import           Data.Foldable
import qualified Data.Map            as M
import           Data.Maybe
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
        msg = "Discard old version record of " ++ T.unpack myname
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
listAllVirtuals = foldr bFn M.empty . M.elems . suiteRecords where
  bFn :: SourceRecord -> M.Map VirtualName [BinName] -> M.Map VirtualName [BinName]
  bFn sr = unionWithKey vs' where
    vs' = extractVirtuals sr

putRecord :: SourceRecord -> Suite' ()
putRecord sr = do
  s <- S.get
  let rs = suiteRecords s
  S.put $ s { suiteRecords = M.insert (sname sr) sr rs }
  return ()

findDepends :: Suite -> SourceRecord -> Either [T.Text] [SourceRecord]
findDepends s sr = if null badDeps then Right goodSrcs else Left badNames where
    (goodDeps, badDeps) = partition isJust $ map (reduceDepend s) (buildDepends sr)

    badNames = map (T.pack . show) badDeps
    goodSrcs = map depSrc $ filter (/= IgnoreDepend) $ catMaybes goodDeps

    depSrc :: Depend -> SourceRecord
    depSrc d = fromJust $ findSourceByBinName s (dName d)

-- | @reduceDepend s d@ 根据仓库@s@的状态，
--
-- 1. 若@d@为'Types.OneOfDepend', 则将@d@降级为'Types.SimpleDepend'.
-- 2. 若@d@为'Types.SimpleDepend',
--
--      - 若@s@满足@d@则返回@Just d@
--      - 若@dArchLimit@与@s@不匹配则直接忽略返回@Just IgnoreDepend@
--      - 否则返回@Nothing@
reduceDepend :: Suite -> Depend -> Maybe Depend
reduceDepend s d@SimpleDepend{ dName=n, dArchLimit=aLimit } =
  if | canIgnore -> Just IgnoreDepend
     | hasTheSource -> Just d
     | otherwise -> Nothing where
  hasTheSource = isJust $ bin2srcName s n
  canIgnore = not (null aLimit) && (suiteArch s `notElem` aLimit)
reduceDepend _ IgnoreDepend = Just IgnoreDepend
reduceDepend _ (OneOfDepend []) = Nothing
reduceDepend s (OneOfDepend (d:ds)) = if isJust d' then d' else reduceDepend s (OneOfDepend ds) where
  d' = reduceDepend s d

listAllBinaries :: Suite -> [BinaryRecord]
listAllBinaries = concatMap (M.elems . outputs) . M.elems . suiteRecords

-- | @priorities s@ list the tuple of priority group and its counts
--
-- >>> priorities s
-- [(73,"required"),(82,"important"),(99,"standard"),(2568,""),(14265,"extra"),(40434,"optional")]
priorities :: Suite -> [(Int, T.Text)]
priorities s = sortOn fst $ fmap (length &&& head) $ (group . sort) $ map priority $ listAllBinaries s

-- | stage1 fn 根据fn的返回值来标记初始化时各SourceRecord的shash值
stage1 :: (SourceRecord -> Maybe OutputHash) -> Suite' ()
stage1 fn = do
  s <- S.get
  let trySetSHash sr = case fn sr of
        Nothing -> sr
        Just sh -> sr { shash = Just sh }
      ss' = M.map (trySetSHash) (suiteRecords s)
  S.put s { suiteRecords = ss' }


-- | badSourceRecords __/s/__ 返回仓库__/s/__中无法编译的源码包名列表
badSourceRecords :: Suite -> [SrcName]
badSourceRecords = map sname . filter (isNothing . shash) . M.elems . suiteRecords

-- | stage2 根据stage1的结果，重复尝试计算shash
stage2 :: Suite' ([Int], Int)
stage2 = do
  r <- hardwork
  s <- S.get
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

findSourceByBinName :: Suite -> BinName -> Maybe SourceRecord
findSourceByBinName s n = bin2srcName s n >>= findSourceBySrcName s

findSourceBySrcName :: Suite -> SrcName -> Maybe SourceRecord
findSourceBySrcName s n = M.lookup n $ suiteRecords s

bin2srcName :: Suite -> BinName -> Maybe SrcName
bin2srcName s bn = debugR where
  (xx, vs) = suiteCache s
  debugR = if isNothing result then trace ("Can't find " <> T.unpack bn) result else result

  result = if isJust d then d else v

  d :: Maybe SrcName
  d = M.lookup bn xx

  v :: Maybe SrcName
  v =  join (listToMaybe . mapMaybe (`M.lookup` xx) <$> M.lookup bn vs)


-- | @canBeBuild s sr@ 判断此'Types.SourceRecord'是否可以被直接计算出shash
--
-- 1. @buildDepends sr@ 为空 && @shash sr == Nothing@  --> True
-- 2. forall e in BuildDepends of sr, the shash of e isn't Nothing -> True
-- 3. Otherwie -> False
canBeBuild :: Suite -> SourceRecord -> Bool
canBeBuild s sr = if
  | isJust $ shash sr -> False
  | null . buildDepends $ sr -> True
  | otherwise -> case v of
      Right r -> r
      Left xs -> trace (T.unpack myname ++ " Failed " ++ show xs) False
  where
    myname = sname sr
    v = p2 sr
    p2 sr' = all (isJust . shash) <$> findDepends s sr'

-- | 生成所有零依赖的shash
calc :: Suite' Int
calc = do
  s <- S.get
  let sr = suiteRecords s
      ss = filter (canBeBuild s) $ M.elems sr
  oks <- mapM tryCalc ss
  return $ length oks

tryCalc :: SourceRecord -> Suite' Bool
tryCalc sr = do
  s <- S.get

  let failMsg x = (error ("failed calc " ++ T.unpack (sname sr) ++ ":" ++ show x))

  let sethash h = putRecord (sr { shash = Just h }) >> return True

  let myhash = dscHash $ dsc sr :: OutputHash

  let depHashs = case findDepends s sr of
        Left err -> trace (show err) $ Left err
        Right deps -> case mapM shash deps of
            Nothing -> Left ["HH"]
            Just x  -> Right x

  let jh = T.pack . hashArray . (myhash:) <$> depHashs :: Either [T.Text] OutputHash

  either failMsg sethash jh


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
