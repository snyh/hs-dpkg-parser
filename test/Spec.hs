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
  specHash

prop_sameLengthForPartitionN :: [String] -> Property
prop_sameLengthForPartitionN xs = forAll (choose (0, length xs - 1)) checker where
  checker n = let
    (p1, p2 ) = U.partitionN n (even . length) xs
    in
    length xs == length p1 + length p2

specUtils :: Spec
specUtils = describe "Utils.PartitionN" $
    it "Shouldn't change the input set length" $ property prop_sameLengthForPartitionN


specArchitecture :: Spec
specArchitecture = describe "Compare Architectures" $ do
    it "i386 != amd64" $ ArchName "i386" /= ArchName "amd64"
    it "i386 == any" $ ArchAny == ArchName "i386"
    it "all != any" $ ArchAll /= ArchAny


specHash :: Spec
specHash = describe "hashIt with string" $
    let str = "abc" :: String
        strU = "hello world 你好\n" :: String
        result = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        resultU = "0f8ec95a2d72a48aebfbb2a082b04f3bd53be8d4dba59a8e31e82c504bae50d8"
    in do
      it "Comparing String" $
        hashIt str
        `shouldBe`
        result
      it "Comparing Unicode String" $
        hashIt strU
        `shouldBe`
        resultU

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
        dep = SimpleDepend "liba52-0.7.4" ver [ArchName "i386"]
        ver = VerEQ $ Version 0 "0.7.4" "18"
      in it ("parseDepends: " ++ T.unpack str) $ R.parseDepends str `shouldBe` Right [dep]


