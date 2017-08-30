{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text             as T

import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.QuickCheck

import           Record                as R
import           Types
import           Utils                 as U

main :: IO ()
main = hspec $ do
  specUtils
  specParser
  specArchitecture

prop_sameLengthForPartitionN :: [String] -> Property
prop_sameLengthForPartitionN xs = forAll (choose (0, length xs - 1)) checker where
  checker n = let
    (p1, p2 ) = U.partitionN n (even . length) xs
    in
    length xs == length p1 + length p2

specUtils :: Spec
specUtils = do
  describe "Utils.PartitionN" $ do
    it "Shouldn't change the input set length" $ property prop_sameLengthForPartitionN


specArchitecture :: Spec
specArchitecture = do
  describe "Compare Architectures" $ do
    it "i386 != amd64" $ ArchName "i386" /= ArchName "amd64"
    it "i386 == any" $ ArchAny == ArchName "i386"
    it "all != any" $ ArchAll /= ArchAny

specParser :: Spec
specParser = do
  describe "shouldParse" $
    it "Works on: \"x,y,z,\" ~> R.fields ','" $
       ("x,y,z," :: T.Text) ~> R.fields ','
    `shouldParse` ["x", "y", "z"]


  describe "pLimitArch" $ do
    it "pLimitArch" $
      ("[i386 !!amd64 !ia64]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchName "i386", ArchName "amd64", ArchIsNot (ArchName "ia64")]

    it "all" $
      ("[all]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchAll]

    it "not . not name and any" $
      ("[!!!!i386 any]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchName "i386", ArchAny]


  describe "R.parseXXX" $ do
    let str = "liba52-0.7.4 (= 0.7.4-18) [i386] <buildd>"
        dep = Depend "liba52-0.7.4" ver [ArchName "i386"]
        ver = VerEQ $ Version 0 "0.7.4" "18"
      in it ("parseDepends: " ++ T.unpack str) $ R.parseDepends str `shouldBe` Right [dep]


