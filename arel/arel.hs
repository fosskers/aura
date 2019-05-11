{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Options.Generic
import           Prelude hiding (FilePath)
import           Shelly

---

newtype Env = Env { release :: T.Text } deriving (Generic, ParseRecord)

projectDir :: FilePath
projectDir = "/home/colin/code/haskell/aura/"

aurDir :: FilePath
aurDir = "/home/colin/code/haskell/aura/aur-pkgs/aura/"

main :: IO ()
main = do
  env <- getRecord "AREL - Create an Aura release"
  shelly $ errExit False $ do
    cd projectDir
    makeNewPkgFile env
    alterPKGBUILD
    makeSrcInfo
    echo "Done."

makeNewPkgFile :: Env -> Sh ()
makeNewPkgFile (Env v) = do
  run_ "stack" ["build", "aura"]
  run_ "stack" ["sdist", "aura"]
  cp (tarballPath v) aurDir

-- TODO Use flags to dump the tarball / binary to a static location.
-- | The location of a built release tarball, the output of `stack sdist`.
-- Beware: the `cabal` version is not static.
-- See: https://github.com/commercialhaskell/stack/issues/3568
tarballPath :: Text -> FilePath
tarballPath v = fromText $ "aura/.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/aura-" <> v <> ".tar.gz"

alterPKGBUILD :: Sh ()
alterPKGBUILD = do
  cd aurDir
  md5 <- run "makepkg" ["-g"]
  pb  <- T.lines <$> readfile "PKGBUILD"
  let news = map (\l -> if T.isPrefixOf "md5sums=" l then md5 else l) pb
  writefile "PKGBUILD" $ T.unlines news

makeSrcInfo :: Sh ()
makeSrcInfo = run "makepkg" ["--printsrcinfo"] >>= writefile ".SRCINFO"
