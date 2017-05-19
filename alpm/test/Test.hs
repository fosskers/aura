{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alpm
import Data.Text (Text, unpack)
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "ALPM Bindings"
  [ testGroup "Misc."
    [ testCase "ALPM Version" $ version @?= "10.0.1"
    , testCase "Initialize Handler" initT
    ]
  ]

assertRight :: Either Text a -> Assertion
assertRight (Left e) = assertFailure $ unpack e
assertRight (Right _) = pure ()

initT :: Assertion
initT = do
  handle <- initialize "/" "/var/lib/pacman/"
  case handle of
    Right h -> do
      rootPath h @?= "/"
      dbPath h @?= "/var/lib/pacman/"
      lockfile h @?= "/var/lib/pacman/db.lck"
      close h >>= \res -> res @?= 0
    Left e  -> assertFailure $ unpack e
