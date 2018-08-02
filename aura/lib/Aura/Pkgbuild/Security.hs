{-# LANGUAGE OverloadedStrings, TupleSections, TypeApplications #-}

-- |
-- Module    : Aura.Pkgbuild.Security
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Analyse PKGBUILDs for potentially malicious bash code.

module Aura.Pkgbuild.Security
  ( BannedTerm(..), BanCategory(..)
  , parsedPB, bannedTerms
  , reportExploit
  ) where

import           Aura.Languages
import           Aura.Types (Pkgbuild(..), Language)
import           Aura.Utils (strictText)
import           BasePrelude hiding (Last, Word)
import           Control.Error.Util (hush)
import           Data.Generics.Product
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc (Doc)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import           Language.Bash.Parse (parse)
import           Language.Bash.Syntax
import           Language.Bash.Word (Word, unquote)
import           Control.Lens

---

-- | A bash term which should never appear in a PKGBUILD. If one does, it's
-- either a sign of maintainer negligence or malicious behaviour.
data BannedTerm = BannedTerm T.Text BanCategory deriving (Eq, Ord, Show)

-- | The reason why the bash term is black-listed.
data BanCategory = Downloading | ScriptRunning | Permissions deriving (Eq, Ord, Show)

blacklist :: M.Map T.Text BannedTerm
blacklist = M.fromList $ downloading <> running <> permissions
  where downloading = map (\t -> (t, BannedTerm t Downloading)) ["curl", "wget", "git", "rsync", "scp"]
        running     = map (\t -> (t, BannedTerm t ScriptRunning)) ["sh", "bash", "source", ".", "eval", "zsh", "fish"]
        permissions = map (\t -> (t, BannedTerm t Permissions)) ["sudo", "ssh"]

-- | Attempt to parse a PKGBUILD. Should succeed for all reasonable PKGBUILDs.
parsedPB :: Pkgbuild -> Maybe List
parsedPB (Pkgbuild pb) = hush . parse "PKGBUILD" . T.unpack $ strictText pb  -- TODO wasteful conversion!

-- | Discover any banned terms lurking in a parsed PKGBUILD, paired with
-- the surrounding context lines.
-- bannedTerms :: List -> [(Statement, NonEmptySet BannedTerm)]
-- bannedTerms (List sms) = mapMaybe exploit sms

bannedTerms :: List -> [(ShellCommand, BannedTerm)]
bannedTerms = mapMaybe bannedCommand . simpleCommands

exploit :: Statement -> Maybe (Statement, NonEmptySet BannedTerm)
exploit s = fmap (s,) . NES.fromSet . S.fromList . mapMaybe banned $ s ^.. types @Word

banned :: Word -> Maybe BannedTerm
banned w = show w `trace` M.lookup (T.pack $ unquote w) blacklist

simpleCommands :: List -> [ShellCommand]
simpleCommands l = l ^.. types @ShellCommand . filtered p
  where p (SimpleCommand _ _) = True
        p _ = False

bannedCommand :: ShellCommand -> Maybe (ShellCommand, BannedTerm)
bannedCommand s@(SimpleCommand _ (c:_)) = "HERE!" `trace` ((s,) <$> banned c)
-- bannedCommand s@(SimpleCommand _ _) = show s `trace` Nothing  -- All assignments
bannedCommand _ = Nothing

------------
-- REPORTING
------------

-- | Dispatch different error messages depending on the category of a `BannedTerm`.
reportExploit :: BannedTerm -> (Language -> Doc AnsiStyle)
reportExploit (BannedTerm t bc) = case bc of
  Downloading   -> security_2 t
  ScriptRunning -> security_3 t
  Permissions   -> security_4 t
