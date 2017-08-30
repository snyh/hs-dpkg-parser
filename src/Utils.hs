{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils (
  readAny
  ,partitionN
  ,hashArray
  ,hashIt
  ,fallback
  ,jq
  ,jq'
  ,readDefault
  ,loadObject
  ,storeObject
) where

import           Codec.Compression.GZip     as GZ
import           Crypto.Hash                (Digest, SHA256, hash)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy       as BL
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Store                 (Store, decode, encode)
import           Data.String                (IsString)
import           Network.Curl.Download.Lazy (openLazyURI)
import           System.Directory           (removeFile)
import           System.FilePath
import           System.IO
import           System.Process             (callCommand)
import           Text.Printf
import           Text.Read

hashArray :: (Show a, Monoid a, Ord a) => [a] -> String
hashArray xs = hashIt $ foldr1 mappend $ nub $ sort xs

hashIt :: (Show a) => a -> String
hashIt s = show (hash $ pack $ show s :: Digest SHA256)


openURI' :: FilePath -> IO BL.ByteString
openURI' f = openLazyURI f >>= \case
  (Right content) -> return content
  (Left err)      -> error ("handle \"" <> f <> "\" :" <> err)

storeObject :: Store a => a -> FilePath -> IO ()
storeObject s f = BS.writeFile f (encode s)

loadObject :: Store a => FilePath -> IO a
loadObject f =  do
  c <- BS.readFile f
  case decode c of
    (Left err) -> error $ show err
    (Right s)  -> return s


readAny :: FilePath -> IO BL.ByteString
readAny f = let
  content = reader f f
  reader :: FilePath -> (FilePath -> IO BL.ByteString)
  reader ('h':'t':'t':'p':':':_)     = openURI'
  reader ('h':'t':'t':'p':'s':':':_) = openURI'
  reader _                           = BL.readFile
  in
    case takeExtension f of
      ".gz" -> GZ.decompress <$> content
      _     -> content


fallback :: (Eq a, IsString a) => a -> a -> a
fallback x fb = if x == "" then fb else x

jq :: (JSON.ToJSON a) => a -> IO ()
jq = jq' "."

jq' :: (JSON.ToJSON a) => String -> a -> IO ()
jq' expr s = do
  (fpath, h) <- openTempFile "/tmp" "__jq__.json"
  BL.hPut h $ JSON.encode s
  hClose h
  callCommand (printf "jq  \"%s\" %s " expr fpath)
  removeFile fpath

partitionN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
partitionN n p = foldl' fn ([],[]) where
  fn (s1, s2) x
    | n > 0 && (length s1 >= n) = (s1, x:s2)
    | p x = (x:s1, s2)
    | otherwise = (s1, x:s2)


readDefault :: Read a => a -> String -> a
readDefault def str = fromMaybe def $ readMaybe str
