{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import qualified Data.Map                        as M
import           Data.Maybe
import           Suite
import           System.Console.CmdArgs.Implicit
import           System.IO.Unsafe
import           Workaround                      (buildCache)


myProfile :: SuiteProfile
myProfile = M.fromList [ ("Arch", "amd64") ]

main = do
  c <- cmdArgs myArgs
  putStrLn "Start parsing...."
  buildCache (srcFile c) (binFile c) (outFile c) myProfile

data DRepo = DRepo {
  srcFile  :: String
  ,binFile :: String
  ,outFile :: String
  } deriving (Show, Data, Typeable)

myArgs :: DRepo
myArgs = DRepo {
  srcFile = "./Sources"
    &= help "raw Sources control file"
    &= typFile

  ,binFile = "./Packages"
    &= help "raw Packages control file"
    &= typFile

  ,outFile = "./ok.dat"
    &= help "the output files"
    &= typFile
  }
  &= summary "DRepo v1"

gg :: SrcName -> SourceRecord
gg sn = fromJust $ findSource s sn
s = unsafePerformIO $ loadSuite "./ok.dat"
