{-# LANGUAGE OverloadedStrings, TupleSections #-}

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
import           BasePrelude hiding (Last, Word)
import           Control.Error.Util (hush)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc (Doc)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Language.Bash.Cond as Cond
import           Language.Bash.Parse (parse)
import           Language.Bash.Syntax
import           Language.Bash.Word (Word, unquote)
import           Lens.Micro

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
parsedPB (Pkgbuild pb) = hush . parse "PKGBUILD" $ T.unpack pb

-- | Discover any banned terms lurking in a parsed PKGBUILD, paired with
-- the surrounding context lines.
bannedTerms :: List -> [(Statement, NonEmptySet BannedTerm)]
bannedTerms (List sms) = mapMaybe exploit sms

exploit :: Statement -> Maybe (Statement, NonEmptySet BannedTerm)
exploit s = fmap (s,) . NES.fromSet . S.fromList . mapMaybe banned $ s ^.. terms

banned :: Word -> Maybe BannedTerm
banned w = M.lookup (T.pack $ unquote w) blacklist

------------
-- REPORTING
------------

reportExploit :: BannedTerm -> (Language -> Doc AnsiStyle)
reportExploit (BannedTerm t bc) = case bc of
  Downloading   -> security_2 t
  ScriptRunning -> security_3 t
  Permissions   -> security_4 t

---------
-- OPTICS
---------

statements :: List -> [Statement]
statements (List sms) = sms

-- terms :: SimpleGetter Statement Word
terms = andor . to pipeline . each . command . each . shellcommand . to allwords . each

andor :: Lens' Statement AndOr
andor = lens (\(Statement ao _) -> ao) (\(Statement _ lt) ao' -> Statement ao' lt)

pipeline :: AndOr -> [Pipeline]
pipeline (Last pl)   = [pl]
pipeline (And pl ao) = pl : pipeline ao
pipeline (Or  pl ao) = pl : pipeline ao

command :: Lens' Pipeline [Command]
command = lens commands (\pl cs' -> pl { commands = cs' })

shellcommand :: Lens' Command ShellCommand
shellcommand = lens (\(Command sc _) -> sc) (\(Command _ r) sc' -> Command sc' r)

cond :: Cond.CondExpr a -> [a]
cond (Cond.Unary _ a)    = [a]
cond (Cond.Binary a _ b) = [a, b]
cond (Cond.Not ce)       = cond ce
cond (Cond.And as bs)    = cond as <> cond bs
cond (Cond.Or as bs)     = cond as <> cond bs

allwords :: ShellCommand -> [Word]
allwords sc = case sc of
  SimpleCommand _ ws -> ws
  AssignBuiltin w es -> w : (es ^.. each . _Right)
  FunctionDef _ ls   -> ls ^.. ts
  Coproc _ c         -> c ^.. shellcommand . to allwords . each
  Subshell ls        -> ls ^.. ts
  Group ls           -> ls ^.. ts
  Arith _            -> []
  Cond ce            -> cond ce
  For _ _ ls         -> ls ^.. ts
  ArithFor _ ls      -> ls ^.. ts
  Select _ _ ls      -> ls ^.. ts
  Case _ cc          -> cc ^.. each . to (\(CaseClause _ ls _) -> ls) . ts
  If as bs cs        -> (as ^.. ts) <> (bs ^.. ts) <> (cs ^.. _Just . ts)
  Until as bs        -> (as ^.. ts) <> (bs ^.. ts)
  While as bs        -> (as ^.. ts) <> (bs ^.. ts)
  where ts = to statements . each . terms
