{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Aura.Core
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
  ]
