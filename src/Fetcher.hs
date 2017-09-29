module Fetcher (
  SuiteConfig(..)
  ,downloadSuite
  ,fetchAndMerge
  ,fetchDFile
  ) where

import           Data.Monoid
import           Utils

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Record                     (Record, parseString, showRecord)
import           System.IO
import           Text.Printf

data SuiteConfig = SuiteConfig {
    host      :: String
    ,codeName :: String
    ,arch     :: String
  } deriving (Show, Eq)


bl2text :: BL.ByteString -> T.Text
bl2text = T.pack . BL.unpack

type Hash = String
type DFile = (FilePath, Maybe Hash)

simpleDFile :: FilePath -> DFile
simpleDFile f = (f, Nothing)

fetchDFile :: DFile -> IO BL.ByteString
fetchDFile (fp, Nothing) = readAny fp
fetchDFile (fp, Just h) = do
  bs <- readAny fp
  let rh  = hashIt bs
  if rh /= h then
    error $ printf "hash mismatch, expect %s, but got %s. when fetch %s" h rh fp
    else
    return bs

checkDFile :: DFile -> IO ()
checkDFile (f, Nothing) = return ()
checkDFile (f, Just h)  = void $ fetchDFile (f, Just h)

fetchAndMerge :: [DFile] -> DFile -> IO ()
fetchAndMerge ins out@(f, h) = checkDFile out >> doIt where
  doIt = withFile f WriteMode $ \h -> mapM_ (fetchDFile >=> BL.hPutStrLn h) ins

downloadSuite :: SuiteConfig -> IO (FilePath, FilePath)
downloadSuite cfg = let
  distFile :: String -> String -> String
  distFile a b = host cfg <> "/dists/" <> codeName cfg <> "/" <> a <> "/" <> b

  components = [ "main", "contrib", "non-free" ]

  srcList = distFile <$> components <*> ["source/Sources.gz"]
  binList = distFile <$> components <*> ["binary-amd64/Packages.gz"]

  release = host cfg <> "/dists/" <> codeName cfg <> "/Release"

  handle :: Record -> Int
  handle r = length r

  (f1, f2) = (simpleDFile "ttt_Sources", simpleDFile "ttt_Packages")
  in do
    print srcList
    fetchAndMerge (map simpleDFile srcList) f1
    print binList
    fetchAndMerge (map simpleDFile binList) f2
    -- bs <- readAny release
    -- case parseString (bl2text bs) of
    --   Left err    -> error err
    --   Right (r:_) -> print $ handle r
    --   _           -> error $ "Parse release failed with " <> release
    return (fst f1, fst f2)
