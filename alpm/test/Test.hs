{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alpm
import Alpm.Internal.Package (alpm_pkg_vercmp)
import Alpm.Internal.Enums
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
  , testGroup "Enums"
    [ testCase "validations" validationsT
    ]
  , testGroup "Misc."
    [ testCase "ALPM Version" $ alpmVersion @?= SemVer 10 0 2 [] []
    , testCase "Initialize Handler" initT
    ]
  ]
  where successive = concatMap comparisons [ slnp, ml, wpr, wprml, mpri, ftmp, adad ]
        pairs = map (uncurry vercmpT) $ concat [ anv, adv, epochs, eoo, espp ]


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

validationsT :: Assertion
validationsT = validations (mconcat xs) @?= xs
  where xs = [vm_none, vm_md5, vm_sha256, vm_sig ]

comparisons :: [Text] -> [TestTree]
comparisons vs = zipWith vercmpT vs $ tail vs

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
      , ("2.0a", "2.0.a")
      ]

-- | Alpha dots and dashes
adad :: [Text]
adad = [ "1.5-1", "1.5.b" ]

epochs :: [(Text, Text)]
epochs = [ ("0:1.0", "0:1.1")
         , ("1:1.0", "2:1.1")
         ]

-- | Epoch + sometimes present pkgrel
espp :: [(Text, Text)]
espp = [ ("0:1.0-1", "1:1.0")
       , ("0:1.1-1", "1:1.0-1")
       ]

-- | Epoch included on one version.
eoo :: [(Text, Text)]
eoo = [ ("0:1.0", "1.1")
      , ("1.0", "0:1.1")
      , ("1.0", "1:1.0")
      , ("1.1", "1:1.0")
      , ("1.1", "1:1.1")
      ]

fromRight :: Either t t1 -> t1
fromRight (Right a) = a
