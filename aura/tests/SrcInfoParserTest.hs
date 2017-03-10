{-

Copyright 2017 Jiehong Ma <email@majiehong.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module SrcInfoParserTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.String
import Data.List
import Data.Text
import SrcInfoParser
import Prelude
import SrcInfoType

tests :: TestTree
tests = testGroup "Src Info Parser"
  [ testCase "Line No Space" lineNoSpace
  , testCase "Line With Space" lineSpace
  , testCase "Line With Version =" lineVersionEqual
  , testCase "Line With Version >=" lineVersionGreaterThan
  , testCase "Line With Version <" lineVersionLowerThan
  , testCase "Line With Arch" lineArch
  , testCase "Line With Arch x64" lineArchSixtyFour
  , testCase "Line With URL" lineUrl
  , testCase "Line With CheckSum" lineCheckSum
  , testCase "Multiple Lines" multipleLine
  , testCase "Combine Empty Maybe" combineJustListEmpty
  , testCase "Combine Left Empty Line" combineJustListLeftEmpty
  , testCase "Combine Right Empty Line" combineJustListRightEmpty
  , testCase "Combine Full Line" combineJustListFull
  , testCase "Add 2 empty SrcInfo" addNothing
  , testCase "Add 1 empty SrcInfo" addOneNothing
  , testCase "Add 2 SrcInfo" addTwoSrcInfo
  , testCase "Merge [SrcInfo]" mergeSrcInfo
  , testCase "Nominal Example" nominalExample
  ]

lineNoSpace :: Assertion
lineNoSpace = do
  let expected = Line {parameter = "name", value = "value"}
  let lineToParse = fromString (parameter expected ++ "=" ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineSpace :: Assertion
lineSpace = do
  let expected = Line {parameter = "name", value = "value"}
  let lineToParse = fromString (" " ++ parameter expected ++ " = " ++ value expected ++ "  ")
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineVersionEqual :: Assertion
lineVersionEqual = do
  let expected = Line {parameter = "name", value = "value=2.3"}
  let lineToParse = fromString (" " ++ parameter expected ++ "=" ++ value expected ++ "  ")
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineVersionGreaterThan :: Assertion
lineVersionGreaterThan = do
  let expected = Line {parameter = "name", value = "value>=2.3"}
  let lineToParse = fromString (" " ++ parameter expected ++ "=" ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineVersionLowerThan :: Assertion
lineVersionLowerThan = do
  let expected = Line {parameter = "name", value = "value<2.3"}
  let lineToParse = fromString (" " ++ parameter expected ++ "=" ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineArch :: Assertion
lineArch = do
  let expected = Line {parameter = "arch", value = "i686"}
  let lineToParse = fromString ("\t" ++ parameter expected ++ " = " ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineArchSixtyFour :: Assertion
lineArchSixtyFour = do
  let expected = Line {parameter = "arch", value = "x86_64"}
  let lineToParse = fromString ("\t" ++ parameter expected ++ " = " ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineUrl :: Assertion
lineUrl = do
  let expected = Line {parameter = "source", value = "git+https://github.com/user/project.git#tag=name-0.35"}
  let lineToParse = fromString (" " ++ parameter expected ++ "=" ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

lineCheckSum :: Assertion
lineCheckSum = do
  let expected = Line {parameter = "sha512sums", value = "fb50ba9f6d5ea8aa81be8f1823040a175809e844e44262"}
  let lineToParse = fromString (" " ++ parameter expected ++ "=" ++ value expected)
  let actual = parseSingleLine lineToParse
  assertEqual "" expected actual

multipleLine :: Assertion
multipleLine = do
  let expected = [ Line {parameter = "sha512sums", value = "fb50ba9f6d5ea8aa81be8f1823040a175809e844e44262"}
                 , Line {parameter = "name", value = "val"}]
  let linesToParse = Data.String.lines (" " ++ parameter (Data.List.head expected) ++ "=" ++ value (Data.List.head expected)
                            ++ "\n" ++ parameter (expected !! 1) ++ "=" ++ value (expected !! 1))
  let actual = Prelude.map parseSingleLine linesToParse
  assertEqual "" expected actual

combineJustListEmpty :: Assertion
combineJustListEmpty = do
  let actual = combineJustList Nothing Nothing
  assertEqual "" Nothing actual

combineJustListLeftEmpty :: Assertion
combineJustListLeftEmpty = do
  let expected = Just [pack "a", pack "b"]
  let actual = combineJustList Nothing expected
  assertEqual "" expected actual

combineJustListRightEmpty :: Assertion
combineJustListRightEmpty = do
  let expected = Just [pack "a", pack "b"]
  let actual = combineJustList expected Nothing
  assertEqual "" expected actual

combineJustListFull :: Assertion
combineJustListFull = do
  let expected = Just [pack "a", pack "b", pack "c", pack "d"]
  let actual = combineJustList (Just [pack "a", pack "b"]) (Just [pack "c", pack "d"])
  assertEqual "" expected actual

addNothing :: Assertion
addNothing = do
  let expected = srcInfoData
  let actual = add srcInfoData srcInfoData
  assertEqual "" expected actual

addOneNothing :: Assertion
addOneNothing = do
  let expected = srcInfoData {arch = Just [pack "one"]}
  let actual = add srcInfoData srcInfoData {arch = Just [pack "one"]}
  assertEqual "" expected actual

addTwoSrcInfo :: Assertion
addTwoSrcInfo = do
  let expected = srcInfoData {arch = Just [pack "one", pack "middle", pack "two"]}
  let actual = add srcInfoData {arch = Just [pack "one", pack "middle"]} srcInfoData {arch = Just [pack "two"]}
  assertEqual "" expected actual

mergeSrcInfo :: Assertion
mergeSrcInfo = do
  let expected = srcInfoData {arch = Just [pack "one", pack "middle", pack "two"]}
  let actual = merge [srcInfoData {arch = Just [pack "one"]}, srcInfoData {arch = Just [pack "middle"]}, srcInfoData {arch = Just [pack "two"]}]
  assertEqual "" expected actual

exampleLines :: [String]
exampleLines = [ "# Generated by makepkg 4.2.1"
               , "              # Tue Jun  9 20:30:57 UTC 2015"
               , "pkgbase = brise-extra"
               , "\tpkgdesc = Rime schema repository with extra methods installed (array30, scj6, stenotype, traditional wubi)"
               , "\tpkgver = 0.35"
               , "\tpkgrel = 2"
               , "\turl = http://code.google.com/p/rimeime/"
               , "\tarch = i686"
               , "\tarch = x86_64"
               , "\tlicense = GPL3"
               , "\tmakedepends = cmake"
               , "\tmakedepends = git"
               , "\tmakedepends = librime>=1.2"
               , "\tprovides = librime-data"
               , "\tprovides = brise"
               , "\tconflicts = librime<1.2"
               , "\tconflicts = ibus-rime<1.2"
               , "\tconflicts = brise"
               , "\tsource = git+https://github.com/lotem/brise.git#tag=brise-0.35"
               , "\tsource = Makefile.patch"
               , "\tsha512sums = SKIP"
               , "\tsha512sums = fb50ba9f6d5ea8aa81be8f1823040a175809e844e4426228188749ef178dec496aa44571cd5980845bc5392a1c1476871c676f32ba3ab917ddc1c01008ee6018"
               , ""
               , "pkgname = brise-extra"]

fullExample :: String
fullExample = Data.List.intercalate "\n" exampleLines

fullExampleExpected :: SrcInfo
fullExampleExpected = srcInfoData { name        = Just (pack "brise-extra")
                                  , version     = Just (pack "0.35")
                                  , release     = Just 2
                                  , epoch       = Nothing
                                  , arch        = Just [pack "i686", pack "x86_64"]
                                  , licenses    = Just [pack "GPL3"]
                                  , makeDepends = Just [pack "cmake", pack "git", pack "librime>=1.2"]
                                  , provides    = Just [pack "librime-data", pack "brise"]
                                  , conflicts   = Just [pack "librime<1.2", pack "ibus-rime<1.2", pack "brise"]
                                  , sources     = Just [pack "git+https://github.com/lotem/brise.git#tag=brise-0.35", pack "Makefile.patch"]
                                  , md5sums     = Nothing
                                  , sha1sums    = Nothing
                                  , sha224sums  = Nothing
                                  , sha256sums  = Nothing
                                  , sha384sums  = Nothing
                                  , sha512sums  = Just [pack "SKIP", pack "fb50ba9f6d5ea8aa81be8f1823040a175809e844e4426228188749ef178dec496aa44571cd5980845bc5392a1c1476871c676f32ba3ab917ddc1c01008ee6018"]
                                  }

nominalExample :: Assertion
nominalExample = do
  let actual = parseConfig fullExample
  -- print (Prelude.map parseSingleLine (Data.String.lines fullExample))
  assertEqual "" fullExampleExpected actual
