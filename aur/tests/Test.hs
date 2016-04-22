{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linux.Arch.Aur
import Test.Tasty
import Test.Tasty.HUnit

---

suite :: TestTree
suite = testGroup "RPC Calls"
  [ testCase "info on existing package" infoTest
  , testCase "info on nonexistant package" infoTest'
  , testCase "search" searchTest
  ]

infoTest :: Assertion
infoTest = info ["aura"] >>= assert . not . null

infoTest' :: Assertion
infoTest' = info ["aura1234567"] >>= assert . null

searchTest :: Assertion
searchTest = search "aura" >>= assert . not . null

main :: IO ()
main = defaultMain suite
