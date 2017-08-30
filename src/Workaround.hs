{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workaround(
  buildCache
  ) where


import           Control.Exception
import qualified Data.Map          as M
import           Data.Monoid
import qualified Data.Text         as T
import qualified Record            as R
import           Suite             (Suite, SuiteProfile, buildSuite)
import           Text.Printf
import           Types
import           Utils

srcKeys :: [T.Text]
srcKeys = [
  "Package"
  ,"Version"
  , "Binary"
  , "Package-List"
  , "Checksums-Sha256"
  , "Architecture"
  , "Build-Depends"
  , "Build-Depends-Arch"
  , "Build-Depends-Indep"
  , "Build-Conflicts"
  , "Build-Conflicts-Arch"
  , "Build-Conflicts-Indep"
  ]

binKeys :: [T.Text]
binKeys = [
  "Package"
  ,"SHA256"
  ,"Size"
  ,"Filename"
  ,"Version"
  ,"Source"
  ,"Provides"
  ,"Depends"
  ,"Pre-Depends"
  ,"Breaks"
  ,"Conflicts"
  , "Architecture"
  , "Replaces"
  ]

type BinRecord = R.Record
type SrcRecord = R.Record

type RawVersion = T.Text
type DependLine = T.Text
type ProvideLine = T.Text

matchTheSrc :: SrcName -> RawVersion -> BinRecord -> Bool
matchTheSrc srcName srcVersion pkg = pkgSrcName == srcName && pkgSrcVersion == srcVersion where
  pkgName = R.get "Package" pkg
  defaultVersion = R.get "Version" pkg
  rawSrcStr = R.get "Source" pkg
  (pkgSrcName, pkgSrcVersion) = case R.parseBinSrc rawSrcStr defaultVersion of
    Left _  -> (pkgName, defaultVersion)
    Right t -> t

merge :: [SrcRecord] -> [BinRecord] -> [R.Record]
merge [] _ = []
merge x [] = x
merge (theSrc:otherSrcs) allPkgs = buildOne : merge otherSrcs otherPkgs where
  !buildOne = merge' theSrc thesePkgs

  srcVersion = R.get "Version" theSrc
  srcName = R.get "Package" theSrc

  bins = R.get "Binary" theSrc
  numOfThesePkg = 1 + T.count "," bins

  (!thesePkgs, !otherPkgs) = partitionN numOfThesePkg pFn allPkgs where
    pFn = matchTheSrc srcName srcVersion


merge' :: SrcRecord -> [BinRecord] -> R.Record
merge' sr bs = sr'' where
  (bd, bp) = parseBinaryPackage bs
  sr' = R.setMultilines sr "BinDepends" bd
  sr'' = R.setMultilines sr' "BinProvides" bp

  parseBinaryPackage :: [BinRecord] -> ([DependLine], [ProvideLine])
  parseBinaryPackage = foldr fn ([], []) where
    fn :: BinRecord -> ([DependLine], [ProvideLine]) -> ([DependLine], [ProvideLine])
    fn b (ds, ps) = (ds', ps') where
      _name = R.get "Package" b

      ds' = if v == "" then ds else dep:ds where
        v = R.get "Depends" b
        dep = _name <> " " <> v

      ps' = if v == "" then ps else p:ps where
        v = R.get "Provides" b
        p = _name <> " " <> v


parseRawPackages :: FilePath -> FilePath -> IO ([SrcRecord], [BinRecord], [R.Record])
parseRawPackages fsrc fbin = let
  r1 :: IO (Either String [R.Record])
  r1 = R.parseRecords srcKeys fsrc
  r2 = R.parseRecords binKeys fbin
  in do
    (Right srcs) <- r1
    printf "%s has been parsed to %d source records\n" fsrc (length srcs)
    (Right bins) <- r2
    printf "%s has been parsed to %d binary records\n" fbin (length bins)
    return (srcs, bins, merge srcs bins)

buildCache :: FilePath -> FilePath -> FilePath -> SuiteProfile -> IO Suite
buildCache fsrc fbin fout profile = do
  strOrExc <- try (loadObject fout)
  case strOrExc of
      (Left (_::SomeException)) -> doBuilding
      (Right s)                 -> return s

  where
    doBuilding :: IO Suite
    doBuilding = do
      (_, bins, !rs) <- parseRawPackages fsrc fbin
      putStrLn $ "Start building Suite. -" ++ show (length rs)
      R.storeRecords (fout++".raw") rs
      storeObject (buildDebDownloadCache bins)  (fout ++ ".urlCache")

      let s = buildSuite profile rs
      storeObject s fout
      putStrLn $ "Stroing result to " ++ fout
      return s

buildDebDownloadCache :: [BinRecord] -> M.Map BinName UrlFile
buildDebDownloadCache = foldr build M.empty where
  build :: BinRecord -> M.Map BinName UrlFile -> M.Map BinName UrlFile
  build r = M.insert pname info where
    pname = R.get "Package" r
    info = UrlFile
      (R.get "SHA256" r)
      (readDefault 0 $ T.unpack $ R.get "Size" r)
      (R.get "Filename" r)
