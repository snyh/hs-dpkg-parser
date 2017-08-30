{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Record (
  parseOnly,

  Record,
  showRecord,
  parseDepend,
  get,
  getArray,
  setMultilines,
  getMultilines,

  pLimitArch,
  pArchitecture,

  storeRecords,
  parseString,
  parseRecords,
  fields,

  testParse,

  parseVersion,
  parseUrl,
  parsePkgList,
  parseBinProvides,
  parseBinDepends,
  parseDepends,
  parseBinSrc,
) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text       as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Char                  as C
import           Data.Either
import qualified Data.List.Split            as LS
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as S
import           Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Text.IO               as TIO
import           Debug.Trace
import           Prelude                    as P
import           System.IO
import           Text.Printf
import           Types                      as TP
import           Utils                      (readAny)

between :: Parser a -> Parser a -> Parser b -> Parser b
between pOpen pClose p= pOpen *> p <* pClose

testParse :: Parser t -> Text -> Either String t
testParse p = parseOnly (p <* remain) where
  remain = many' anyChar <* endOfInput >>= pp
  pp x = trace (printf "REMAIN:%s\n" (show x)) $ return x

type KeyString = Text
type ValueString = Text
type KeyValue = M.Map KeyString ValueString
type Record = M.Map KeyString ValueString

showRecord :: Record -> Text
showRecord r = T.unlines $ P.map (\(k,v) -> k <> ":" <> v) $ M.toList r

storeRecords :: FilePath -> [Record] -> IO ()
storeRecords f rs = withFile f WriteMode $ \h -> mapM_ (TIO.hPutStrLn h . showRecord) rs

setMultilines :: Record -> KeyString -> [ValueString] -> Record
setMultilines r k vs = if T.null k || P.null vs then r else M.insert k v r where
  v = P.foldl (\ret x -> ret <> "\n " <> x) "" vs

get :: KeyString -> Record -> ValueString
get k r = T.strip $ M.findWithDefault "" k r

getArray :: KeyString -> Text -> Record -> [ValueString]
getArray k sep r = case get k r of
  ""  -> []
  str -> P.map T.strip $ T.splitOn sep str

getMultilines :: KeyString -> Record -> [ValueString]
getMultilines k r = T.lines $ get k r

records :: [Text] -> Parser [Record]
records keys = many1 p where
  p = record keys

-- https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-controlsyntax
record :: [Text] -> Parser Record
record keys = p >>= \x -> if P.null x then fail "empty string when parsing record" else return x where
  p = P.foldr mappend M.empty <$> manyTill (keyvalue keys) (endOfInput <|> void (many1 endOfLine))

-- The field name is composed of US-ASCII characters excluding control characters, space, and colon
-- (i.e., characters in the ranges U+0021 (!) through U+0039 (9), and U+003B (;) through U+007E (~), inclusive).
-- Field names must not begin with the comment character (U+0023 #), nor with the hyphen character (U+002D -).
fieldKey :: Parser Text
fieldKey = do
  x <- satisfy p1
  xs <- AP.takeWhile p
  return $ cons x xs
  where
  p1 x = x /= '#' && x /= '-' && p x
  p x = let c = C.ord x in (c >= 0x21 && c <= 0x39) || (c >= 0x3b && c <= 0x7e)

fieldValue :: Parser Text
fieldValue = T.stripEnd . T.pack <$> scan' where
  deadChar = '\0'
  scan' = (endOfInput >> return []) <|> do
    p1 <- anyChar
    p2 <- (endOfInput >> return deadChar) <|> peekChar'

    if | p2 == deadChar -> return [p1]
       | p1 == '\n' && (p2 == '\n' || (not . C.isSpace) p2) -> return [p1]
       | otherwise -> do
           xs <- scan'
           return (p1:xs)

keyvalue :: [Text] -> Parser KeyValue
keyvalue keys = (endOfInput >> return M.empty) <|> do
  key <- fieldKey
  void $ char ':'
  field <- fieldValue

  if | field == "" -> return M.empty
     | (not . P.null) keys && key `P.notElem` keys -> return M.empty
     | otherwise -> return $ M.singleton key field


parseString :: Text -> Either String [Record]
parseString = _parse []

parseRecords :: [Text] -> P.FilePath -> P.IO (Either String [Record])
parseRecords keys f = do
  str <- readAny f
  return $ _parse keys (decodeUtf8 $ BL.toStrict str)

_parse :: [Text] -> Text -> Either String [Record]
_parse keys = parseOnly (records keys)

hexDigit :: Parser P.Char
hexDigit = satisfy C.isHexDigit

parseUrl :: Text -> Either String TP.UrlFile
parseUrl = parseOnly p where
  p = TP.UrlFile <$> hash <*> psize <*> path
  sha256Length = 64
  hash = pack <$> trim (AP.count sha256Length hexDigit)
  psize = trim decimal
  path = takeText

trim :: Parser t -> Parser t
trim p = skipSpace *> p <* skipSpace

-- https://www.debian.org/doc/debian-policy/#s-f-package-list
-- (Name, Type, Section, Priority)
parsePkgList :: Text -> Either String (Text, Text, Text, Text)
parsePkgList = parseOnly (trim p) where
  findArch :: [Text] -> Text
  findArch = fromMaybe "" . listToMaybe . P.filter fn where
    fn = isPrefixOf "arch"

  p = do
    fs <- fields ' '
    let len = P.length fs
    if | len == 4 -> return (fs !! 0, fs !! 1, fs !! 3, "")
       | len > 4 -> return (fs !! 0, fs !! 1, fs !! 3, findArch fs)
       | len < 4 -> fail "Invalid package list format"


-- Package names must consist only of lower case letters (a-z), digits (0-9), plus (+) and minus (-) signs, and periods (.). They must be at least two characters long and must start with an alphanumeric character.

pkgNameRange = S.fromList (['a'..'z'] ++ ['0'..'9'] ++ "+-.")
pkgName :: Parser BinName
pkgName = do
  firstC <- peekChar
  if (C.isAlpha <$> firstC) == Just False then
    fail "package name must start with an alphanumeric character"
    else
    takeWhile1 (`S.member` pkgNameRange)


-- An architecture specification consists of one or more architecture names, separated by whitespace. Exclamation  marks  may  be
-- prepended to each of the names, meaning “NOT”.
pLimitArch :: Parser TP.LimitArch
pLimitArch = between (char '[') (char ']') (many1' (trim pArchitecture))

pArchitecture :: Parser Architecture
pArchitecture = reduce <$> p where
  reduce (ArchIsNot (ArchIsNot x)) = reduce x
  reduce x                         = x

  p = (ArchIsNot <$> (char '!' *> p)) <|> pp
  pp = do
    arch <- takeWhile1 (\c -> C.isAlphaNum c || c == '-' || c =='_')
    case arch of
      "any"    -> return ArchAny
      "native" -> return ArchNative
      "all"    -> return ArchAll
      _        -> return $ ArchName arch


pLimitVersion :: Parser TP.LimitVer
pLimitVersion = between (char '(') (char ')') p where
  p = do
    skipSpace
    cm <- pLimit
    skipSpace
    ver <- pversion
    return $ cm ver

parseDepend :: Text -> Either String Depend
parseDepend = parseOnly p2 where
  pVer = option VerAny pLimitVersion
  pArch = option [] pLimitArch
  p1 = do
    n <- trim pkgName
    dVer <- trim pVer
    dArch <- trim pArch
    return $ Depend n dVer dArch

  p2 :: Parser Depend
  p2 = do
    ds <- sepBy p1 (char '|')
    return $ case P.length ds of
      1 -> P.head ds
      _ -> OneOfDepend ds


pLimit :: Parser (Version -> TP.LimitVer)
pLimit = do
  c1 <- oneOf' "<>="
  if | c1 == '=' -> return VerEQ
     | otherwise -> do
         c2 <- oneOf' "<>="
         case c1:[c2] of
           ">>" -> return VerGT
           ">=" -> return VerGTE
           "<<" -> return VerLT
           "<=" -> return VerLTE
           _    -> fail "Unknown comparing"


oneOf' :: [P.Char] -> Parser P.Char
oneOf' xs = satisfy (`P.elem` xs)

pversion :: Parser TP.Version
pversion = let
  alphanumeric = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  cRevision = alphanumeric ++ ".+~"

  upstreamRange :: Parser P.String
  upstreamRange = do
    return $ ':':cRevision
    -- c <- peekChar
    -- traceM $ "HH..." ++ (show c)
    -- case c of
    --   Nothing  -> return cRevision
    --   Just ':' -> return $ ':':cRevision
    --   Just '-' -> return $ '-':cRevision
    --   Just _   -> upstreamRange

  in do
  upAllow <- upstreamRange

  epoch <- option 0 $ try (decimal <* char ':')

  upstream <- option "" $ AP.takeWhile (`elem` upAllow)

  revision <- option "" $ char '-' *> AP.takeWhile (`elem` cRevision)
  return $ TP.Version epoch upstream revision


fields :: P.Char -> Parser [Text]
fields sep = sepBy (AP.takeWhile1 (/= sep)) (AP.takeWhile1 (==sep)) <* p where
  p = do
    c <- peekChar
    if c == Just sep then
      char sep
      else
      return ' '

groupBy :: P.Char -> (Text -> Either String t) -> Parser [t]
groupBy sep p = do
  fs <- fields sep
  case mapM (p . T.strip) fs of
    Left err -> fail err
    Right xs -> return xs

parseBinProvides :: Text -> Either String (BinName, [BinName])
parseBinProvides = parseOnly (trim p) where
  p = (,) <$> trim pkgName <*> sepBy (trim pkgName) sep
  sep = char ','

deps :: Parser [Depend]
deps = Record.groupBy ',' parseDepend
--samples : liba52-0.7.4 (= 0.7.4-18) [i386] <buildd>, we"

parseBinDepends :: Text -> Either String (BinName, [Depend])
parseBinDepends = parseOnly (trim p) where
  p = (,) <$> trim pkgName <*> deps

parseDepends :: Text -> Either String [Depend]
parseDepends = parseOnly deps

parseVersion :: Text -> Either String Version
parseVersion = parseOnly pversion

parseBinSrc :: Text -> Text -> Either String (SrcName, Text)
parseBinSrc str defVer = parseOnly p str where
  p = do
    skipSpace
    n <- pkgName
    skipSpace
    c <- peekChar
    if c == Just '(' then
      do
        void anyChar
        v <- takeWhile1 (/= ')')
        return (n, T.strip v)
      else
      return (n, defVer)
