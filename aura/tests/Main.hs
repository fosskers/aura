{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import SrcInfoParserTest

suite :: TestTree
suite = testGroup "Aura Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [SrcInfoParserTest.tests]

main :: IO ()
main = defaultMain suite
