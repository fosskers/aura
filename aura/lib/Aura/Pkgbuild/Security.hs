{-# LANGUAGE OverloadedStrings, TupleSections #-}

-- |
-- Module    : Aura.Pkgbuild.Security
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Aura.Pkgbuild.Security ( exploits ) where

import           Aura.Types (Pkgbuild(..), Failure(..))
import           BasePrelude hiding (Last, Word)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Language.Bash.Parse (parse)
import           Language.Bash.Syntax
import           Language.Bash.Word (Word, unquote)
import           Lens.Micro

---

newtype BannedTerm = BannedTerm T.Text deriving (Eq, Ord, Show)

blacklist :: M.Map T.Text BannedTerm
blacklist = M.fromList $ map (id &&& BannedTerm) ["curl", "wget", "git", "ssh"]

-- | Determine if a given PKGBUILD contains bash exploits.
-- Will also fail if parsing itself failed.
exploits :: Pkgbuild -> Maybe Failure
exploits (Pkgbuild pb) = case parse "PKGBUILD" $ T.unpack pb of
  Left _ -> Just $ Failure undefined
  Right (List sms) -> undefined $ mapMaybe exploit sms

exploit :: Statement -> Maybe ([BannedTerm], Statement)
exploit s = (,s) <$> traverse banned ws
  where ws = s ^.. andor . to pipeline . each . command . each . shellcommand . to allwords . each

banned :: Word -> Maybe BannedTerm
banned w = M.lookup (T.pack $ unquote w) blacklist

---------
-- OPTICS
---------

andor :: Lens' Statement AndOr
andor = lens (\(Statement ao _) -> ao) (\(Statement _ lt) ao' -> Statement ao' lt)

pipeline :: AndOr -> [Pipeline]
pipeline (Last pl)   = [pl]
pipeline (And pl ao) = pl : pipeline ao
pipeline (Or  pl ao) = pl : pipeline ao

command :: Lens' Pipeline [Command]
command = lens commands (\pl cs -> pl { commands = cs })

shellcommand :: Lens' Command ShellCommand
shellcommand = lens (\(Command sc _) -> sc) (\(Command _ r) sc' -> Command sc' r)

-- The hard one... but that's okay!
allwords :: ShellCommand -> [Word]
allwords = undefined
