{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Data.Maybe
import           Fetcher                         (SuiteConfig (SuiteConfig),
                                                  downloadSuite)
import           Suite
import           System.Console.CmdArgs.Implicit
import           System.IO.Unsafe
import           Types
import           Utils                           (jq, loadObject)
import           Workaround                      (buildCache)

main = do
  c <- cmdArgs myArgs
  putStrLn "Start parsing...."
  buildCache (srcFile c) (binFile c) (outFile c) (ArchName "amd64")

data DRepo = DRepo {
  srcFile  :: String
  ,binFile :: String
  ,outFile :: String
  } deriving (Show, Data, Typeable)


download' = downloadSuite $ SuiteConfig "http://pools.corp.deepin.com/deepin" "panda" "amd64"

defaultOutput = "./ok.dat"

myArgs :: DRepo
myArgs = DRepo {
  srcFile = "./Sources"
    &= help "raw Sources control file"
    &= typFile

  ,binFile = "./Packages"
    &= help "raw Packages control file"
    &= typFile

  ,outFile = defaultOutput
    &= help "the output files"
    &= typFile
  }
  &= summary "DRepo v1"

----------------------------------------------------
gg :: SrcName -> SourceRecord
gg sn = fromJust $ findSourceBySrcName s sn

s :: Suite
s = unsafePerformIO $ loadObject defaultOutput

jj = jq . gg
----------------------------------------------------
