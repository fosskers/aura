{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alpm
import Alpm.Internal.Package (alpm_pkg_vercmp)
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Versions
import Foreign
import Foreign.C.String
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "ALPM Bindings"
  [ testGroup "Packages"
    [ testGroup "Version Comparisons (alpm_pkg_vercmp)" $ successive ++ pairs

    ]
  , testGroup "Misc."
    [ testCase "ALPM Version" $ alpmVersion @?= SemVer 10 0 1 [] []
    , testCase "Initialize Handler" initT
    ]
  ]
  where successive = concatMap comparisons [ slnp, ml, wpr, wprml, mpri, ftmp ]
        pairs = map (uncurry vercmpT) $ concat [ anv, adv ]


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

comparisons :: [Text] -> [TestTree]
comparisons vs = zipWith vercmpT vs $ tail vs

-- TODO: Get alpm's unit tests for `alpm_pkg_ver` and use its samples. So far, it seems
-- pretty brittle since it expects a certain package version layout.

vercmpT :: Text -> Text -> TestTree
vercmpT v0 v1 = testCase (unpack $ v0 <> " < " <> v1) $ do
  v0' <- newCString $ unpack v0
  v1' <- newCString $ unpack v1
  let res = alpm_pkg_vercmp v0' v1'
  res @?= (-1)
  let l = fromRight (parseV v0)
      r = fromRight (parseV v1)
  if l < r
    then pure ()
    else assertFailure $ show l ++ "\n" ++ show r
  free v0' >> free v1'  -- Release allocated memory.

-- Borrowed from `Data.Versions` tests.

semverOrd :: [Text]
semverOrd = [ "1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta"
            , "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1"
            , "1.0.0"
            ]

cabalOrd :: [Text]
cabalOrd = [ "0.2", "0.2.0", "0.2.0.0" ]

versionOrd :: [Text]
versionOrd = [ "0.9.9.9", "1.0.0.0", "1.0.0.1", "2" ]

messOrd :: [Text]
messOrd = [ "10.2+0.93+1-1", "10.2+0.93+1-2", "10.2+0.93+2-1"
          , "10.2+0.94+1-1", "10.3+0.93+1-1", "11.2+0.93+1-1", "12" ]

-- Borrowed from https://git.archlinux.org/pacman.git/tree/test/util/vercmptest.sh

-- | Similar length, no pkgrel
slnp :: [Text]
slnp = [ "1.5.0", "1.5.1" ]

-- | Mixed length
ml :: [Text]
ml = [ "1.5", "1.5.1" ]

-- | With pkgrel
wpr :: [Text]
wpr = [ "1.5.0-1", "1.5.0-2", "1.5.1-1" ]

-- | With pkgrel, mixed length
wprml :: [Text]
wprml = [ "1.5-1", "1.5.1-1", "1.6-1" ]

-- | Mixed pkgrel inclusion
mpri :: [Text]
mpri = [ "1.0-1", "1.1" ]

-- | Alphanumeric versions
anv :: [(Text, Text)]
anv = [ ("1.5b-1", "1.5-1")
      , ("1.5b", "1.5")
      , ("1.5b-1", "1.5")
      , ("1.5b", "1.5.1")
      ]

-- | From the manpage (apparently).
ftmp :: [Text]
ftmp = [ "1.0a", "1.0alpha", "1.0b", "1.0beta", "1.0rc", "1.0" ]

-- | Alpha-dotted versions
adv :: [(Text, Text)]
adv = [ ("1.5", "1.5.a")
      , ("1.5.a", "1.5.b")
      , ("1.5.b", "1.5.1")
      ]

fromRight :: Either t t1 -> t1
fromRight (Right a) = a
