module System (
  System,
  install,
  install',
  bootstrap,
  problems,
) where

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S
import           Debug.Trace
import           Suite
import           Text.Printf

data Status = Installing | Installed | NoExists deriving (Show, Eq)

newtype System' = System' { getsys :: System }
type System = M.Map BinName Status

instance Show System' where
  show s = printf "%d packages have been installed\n%s" num pkgs :: String where
    good = M.filter (==Installed) (getsys s)
    pkgs = M.foldrWithKey (\k v ret -> printf "%s --> %s\n%s" k (show v) ret ::String) "" (bad $ getsys s)
    num = M.size good

bad :: System -> System
bad = M.filter (/=Installed)

install :: System -> Suite -> BinName -> System
install sys s pkgname
  | _hasMarked sys pkgname = sys
  | not (hasThePkg s pkgname) = _mark sys pkgname NoExists
  | otherwise = let
      deps = rdepends s pkgname
      sys' = foldr (\i sss -> install sss s i) (_mark sys pkgname Installing) deps
    in
      _mark sys' pkgname Installed


_mark :: System -> BinName -> Status -> System
_mark sys n msg = M.insert n msg sys

_hasMarked :: System -> BinName -> Bool
_hasMarked s n = M.member n s

bootstrap :: Suite -> [BinName] -> System
bootstrap r = foldr (\n s' -> install s' r n) M.empty



install' :: System -> Suite -> BinName -> Either String System
install' sys s pkgname
  | _hasMarked sys pkgname = Right sys
  | not (hasThePkg s pkgname) = Left ("Can't find the package of " ++ pkgname)
  | otherwise = let
      deps = rdepends s pkgname
      fn i (Left err)= Left err
      fn i (Right sss) = install' sss s i
      sys' = foldr fn (Right (_mark sys pkgname Installing)) deps
    in
      (\x -> _mark x pkgname Installed) <$> sys'

problems :: Suite -> [BinName]
problems s = let
  allDebs = M.keys (listB s)

  xxxx :: BinName -> Maybe String
  xxxx n = case install' M.empty s n of
    Left err  -> Just err
    otherwise ->  Nothing

  fn :: BinName -> [BinName] -> [BinName]
  fn n bads
  --  | n `elem` bads = bads
    | otherwise = if isJust msg then n:bads else bads where msg = xxxx n
  in
  L.nub $ foldr fn [] allDebs
