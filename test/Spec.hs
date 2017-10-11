{-# LANGUAGE OverloadedStrings #-}

import           Data.Either
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as T
import           Record                as R
import           Suite                 (badSourceRecords, bin2srcName,
                                        buildSuite, canBeBuild, empty,
                                        findSourceBySrcName, listAllBinaries,
                                        listAllVirtuals)
import           System.IO.Unsafe
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.QuickCheck
import           Types
import           Utils                 as U
import           Workaround            (bootstrapByDSC)


main :: IO ()
main = hspec $ do
  specUtils
  specParser
  specArchitecture
  specHash
  specSuite

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

    it "i386 == !amd64" $
     ArchName "i386" == ArchIsNot (ArchName "amd64")
      `shouldBe`
      True

    let t = "binutils-x86-64-linux-gnu [!amd64 !i386 !x32] with amd64" :: T.Text
        arch = ArchName "amd64"
      in it ("should ignore " <> T.unpack t) $
         canSafeIgnoreDepend arch <$> parseDepend t
         `shouldBe`
         Right True

    it "shouldn't ignore [i386 amd64] with amd64" $
      canSafeIgnoreDepend (ArchName "amd64") <$> parseDepend "nop [i386 amd64]"
      `shouldBe`
      Right False

    it "should ignore [kfreebsd-any] with amd64" $
      canSafeIgnoreDepend (ArchName "amd64") <$> parseDepend "nop [kfreebsd-any]"
      `shouldBe`
      Right True

  describe "R.parseXXX" $ do
    let str = "liba52-0.7.4 (= 0.7.4-18) [i386] <buildd>"
        dep = SimpleDepend "liba52-0.7.4" ver [ArchName "i386"]
        ver = VerEQ $ Version 0 "0.7.4" "18"
      in it ("parseDepends: " ++ T.unpack str) $ R.parseDepends str `shouldBe` Right [dep]


  describe "comparing version spec" $ do
     let v1 = "3.5.6-4" :: T.Text
         v2 = "3.5.15-2" :: T.Text
       in it (show (v1 <> " should smaller then " <> v2)) $
          R.parseVersion v1 < R.parseVersion v2
          `shouldBe`
          True

specSuite :: Spec
specSuite =
  describe "Parse suite:" $ do
    it "parsing lx_test_data" $
      length testRecords
      `shouldSatisfy`
      (> 4000)

    it "There should have some virtual packages." $
      listAllVirtuals testSuite
      `shouldNotBe`
      empty

    it "The number of bad source records should be lower than 777." $
      length (badSourceRecords testSuite)
      `shouldSatisfy`
      (< 777)

    it "All of binary packages should has a corresponding source package" $
      all (isJust . bin2srcName testSuite) (map bname $ listAllBinaries testSuite)
      `shouldBe`
      True

    let pkg = "file" :: T.Text
      in it ("We should be able to build " ++ T.unpack pkg ++ ".") $
         canBeBuild testSuite <$> findSourceBySrcName testSuite pkg
         `shouldSatisfy`
         isRight . fromJust


testRecords :: [R.Record]
--testRecords = head $ rights [unsafePerformIO $ parseRecords [] "./test/lx_test_data"]
testRecords = head $ rights [unsafePerformIO $ parseRecords [] "./ok.dat.raw"]
testSuite :: Suite
testSuite = buildSuite bootstrapByDSC (ArchName "mips64el") empty testRecords
