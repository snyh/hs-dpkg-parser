{-# LANGUAGE OverloadedStrings #-}

import           Data.Either
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as T
import           Record                as R
import           Suite                 (badSourceRecords, bin2srcName,
                                        buildSuite, deadPackages, empty,
                                        findSourceBySrcName, listAllBinaries,
                                        listAllVirtuals, shouldBuild)
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
    it "shouldn't change the input set length" $ property prop_sameLengthForPartitionN


specArchitecture :: Spec
specArchitecture = describe "Compare Architectures" $ do
    it "i386 != amd64" $ ArchName "i386" /= ArchName "amd64"
    it "i386 == any" $ ArchAny == ArchName "i386"
    it "all != any" $ ArchAll /= ArchAny
    it "i386 == !amd64" $ ArchName "i386" == ArchIsNot (ArchName "amd64")

specHash :: Spec
specHash = describe "hashIt" $
    let str = "abc" :: String
        strU = "hello world 你好\n" :: String
        result = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        resultU = "0f8ec95a2d72a48aebfbb2a082b04f3bd53be8d4dba59a8e31e82c504bae50d8"
    in do
      it "with ascii string" $
        hashIt str
        `shouldBe`
        result
      it "with unicode string" $
        hashIt strU
        `shouldBe`
        resultU

specParser :: Spec
specParser = do
  describe "R.fields" $
    it "\"x,y,z,\" ~> [\"x\", \"y\", \"z\"]" $
       ("x,y,z," :: T.Text) ~> R.fields ','
    `shouldParse` ["x", "y", "z"]

  describe "pLimitArch" $ do
    it "with multiple architecture" $
      ("[i386 !!amd64 !ia64]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchName "i386", ArchName "amd64", ArchIsNot (ArchName "ia64")]

    it "with simple one architecture" $
      ("[all]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchAll]

    it "with multiple negative" $
      ("[!!!!i386 any]" :: T.Text) ~> R.pLimitArch
      `shouldParse`
      [ArchName "i386", ArchAny]

  describe "canSafeIgnoreDependByArch" $ do
    it "should ignore amd64 with [!amd64 !i386 !x32]" $
      canSafeIgnoreDependByArch (ArchName "amd64") <$> parseDepend "nop [!amd64 !i386 !x32]"
      `shouldBe`
      Right True

    it "should ignore amd64 with [kfreebsd-any]" $
      canSafeIgnoreDependByArch (ArchName "amd64") <$> parseDepend "nop [kfreebsd-any]"
      `shouldBe`
      Right True

    it "shouldn't ignore amd64 with [i386 amd64]" $
      canSafeIgnoreDependByArch (ArchName "amd64") <$> parseDepend "nop [i386 amd64]"
      `shouldBe`
      Right False

  describe "canSafeIgnoreDependByProfile" $ do
    it "should ignore ProfileNone with <cross>" $
      canSafeIgnoreDependByProfile ProfileNone <$> parseDepend "nop <cross>"
      `shouldBe`
      Right True

    it "should ignore cross with <!cross>" $
      canSafeIgnoreDependByProfile (ProfileName "cross") <$> parseDepend "nop <!cross>"
      `shouldBe`
      Right True

    it "shouldn't ignore ProfileNone with <!cross>" $
      (canSafeIgnoreDependByProfile ProfileNone <$> parseDepend "nop <!cross>")
      `shouldBe`
      Right False

  describe "R.parseDepends" $ do
    let str = "liba52-0.7.4 (= 0.7.4-18) [i386] <buildd>"
        dep = SimpleDepend "liba52-0.7.4" ver [ArchName "i386"] [ProfileName "buildd"]
        ver = VerEQ $ Version 0 "0.7.4" "18"
      in it ("parsing: " ++ T.unpack str) $
         R.parseDepends str
         `shouldBe`
         Right [dep]


  describe "R.parseVersion" $ do
     let v1 = "3.5.6-4" :: T.Text
         v2 = "3.5.15-2" :: T.Text
       in it (show (v1 <> " should smaller then " <> v2)) $
          R.parseVersion v1 < R.parseVersion v2

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

    let pkg = "file" :: T.Text
      in it ("We should be able to build " ++ T.unpack pkg ++ ".") $
         shouldBuild testSuite <$> findSourceBySrcName testSuite pkg
         `shouldSatisfy`
         isRight . fromJust

    it "There shouldn't have any dead packages" $
      deadPackages testSuite
      `shouldBe`
      []

testRecords :: [R.Record]
--testRecords = head $ rights [unsafePerformIO $ parseRecords [] "./test/lx_test_data"]
testRecords = head $ rights [unsafePerformIO $ parseRecords [] "./ok.dat.raw"]
testSuite :: Suite
testSuite = buildSuite bootstrapByDSC (ArchName "mips64el") empty testRecords
