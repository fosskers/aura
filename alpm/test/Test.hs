{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Alpm

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "ALPM Bindings"
  [ testGroup "Misc."
    [ testCase "ALPM Version" $ alpmVersion @?= "10.0.1"
    ]
  ]
