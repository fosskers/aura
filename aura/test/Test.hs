{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Aura.Types
import BasePrelude
import Data.Versions
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Aura.Core"
    [ testCase "parseDep - python2" $ parseDep "python2" @?= Just (Dep "python2" Anything)
    , testCase "parseDep - python2-lxml>=3.1.0"
      $ parseDep "python2-lxml>=3.1.0" @?= Just (Dep "python2-lxml" . AtLeast . Ideal $ SemVer 3 1 0 [] [])
    , testCase "parseDep - foobar>1.2.3"
      $ parseDep "foobar>1.2.3" @?= Just (Dep "foobar" . MoreThan . Ideal $ SemVer 1 2 3 [] [])
    , testCase "parseDep - foobar=1.2.3"
      $ parseDep "foobar=1.2.3" @?= Just (Dep "foobar" . MustBe . Ideal $ SemVer 1 2 3 [] [])
    ]
  , testGroup "Aura.Types"
    [ testCase "simplepkg"
      $ simplepkg (PackagePath "linux-is-cool-3.2.14-1-x86_64.pkg.tar.xz")
      @?= Just (SimplePkg "linux-is-cool" . Ideal $ SemVer 3 2 14 [[Digits 1]] [])
    , testCase "simplepkg'"
      $ simplepkg' "xchat 2.8.8-19" @?= Just (SimplePkg "xchat" . Ideal $ SemVer 2 8 8 [[Digits 19]] [])
    ]
  ]
