{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Data.Maybe
import           Data.Monoid
import           Fetcher                         (SuiteConfig (SuiteConfig),
                                                  downloadSuite)
import           Suite
import           System.Console.CmdArgs.Implicit
import           System.IO.Unsafe
import           Types
import           Utils                           (jq, loadObject)
import           Workaround                      (buildCache)


_prefix = "debian"
defaultOutput = "./ok.dat"

main = let
  myArgs :: DRepo
  myArgs = DRepo {
    srcFile = _prefix <> "_Sources"
      &= help "raw Sources control file"
      &= typFile

    ,binFile = _prefix <> "_Packages"
      &= help "raw Packages control file"
      &= typFile

    ,outFile = defaultOutput
      &= help "the output files"
      &= typFile
    }
    &= summary "DRepo v1"
  in do
  c <- cmdArgs myArgs
  putStrLn "Start parsing...."
  buildCache (srcFile c) (binFile c) (outFile c) (ArchName "amd64")

data DRepo = DRepo {
  srcFile  :: String
  ,binFile :: String
  ,outFile :: String
  } deriving (Show, Data, Typeable)

download' = downloadSuite _prefix $ SuiteConfig "http://pools.corp.deepin.com/mips64el" "unstable" "mips64el"
downloadP = downloadSuite _prefix $ SuiteConfig "http://pools.corp.deepin.com/debian" "unstable" "amd64"

----------------------------------------------------
gg :: SrcName -> SourceRecord
gg sn = fromJust $ findSourceBySrcName s sn

s :: Suite
s = unsafePerformIO $ loadObject defaultOutput

jj = jq . gg
----------------------------------------------------
