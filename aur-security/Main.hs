{-# LANGUAGE LambdaCase, NamedFieldPuns, DuplicateRecordFields, TupleSections #-}

module Main ( main ) where

import           Aura.Packages.AUR (aurLookup)
import           Aura.Pkgbuild.Security (parsedPB, bannedTerms)
import           Aura.Types
import           BasePrelude
import           Control.Compactable (fmapEither)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (flushTQueue)
import           Control.Concurrent.Throttled (throttleMaybe)
import           Control.Error.Util (note)
import           Data.List.Split (chunksOf)
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.Bash.Pretty (prettyText)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Pretty.Simple (pPrintNoColor)

---

main :: IO ()
main = do
  m   <- newManager tlsManagerSettings
  pns <- sort . map PkgName . T.lines <$> T.readFile "aur-security/packages.txt"
  let grouped = mapMaybe (NES.fromSet . S.fromDistinctAscList) $ chunksOf 50 pns
  putStrLn $ printf "Read %d package names." (length pns)
  throttleMaybe (\_ ps -> aurLookup m ps) grouped >>= \case
    Left _  -> putStrLn "AUR CONNECTION ERROR"
    Right q -> do
      (nons, bs) <- bimap fold (S.filter p . fold) . unzip <$> atomically (flushTQueue q)
      unless (null nons) $ do
        putStrLn "The following weren't identified as AUR packages:"
        traverse_ print nons
      putStrLn "Analysing legal packages..."
      let (unparsed, parsed) = fmapEither f $ toList bs
      unless (null unparsed) $ do
        putStrLn $ printf "The PKGBUILDs of %d packages couldn't be parsed. They were:" (length unparsed)
        traverse_ print unparsed
      let bads = mapMaybe g parsed
      unless (null bads) $ do
        putStrLn $ printf "%d PKGBUILDs contained banned bash terms. They were:" (length bads)
        traverse_ pPrintNoColor bads
      putStrLn "Done."
        where p (Buildable { name, base })     = name == base
              f (Buildable { name, pkgbuild }) = note name . fmap (name,) $ parsedPB pkgbuild
              g = traverse (maybeList . map (first prettyText) . bannedTerms)

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs
