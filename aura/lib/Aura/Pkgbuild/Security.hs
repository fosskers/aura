{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module    : Aura.Pkgbuild.Security
-- Copyright : (c) Colin Woodbury, 2012 - 2020
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
import           Aura.Types (Language, Pkgbuild(..))
import           Aura.Utils (hush)
import           Data.Text.Prettyprint.Doc (Doc)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import           Language.Bash.Parse (parse)
import           Language.Bash.Syntax
import           Language.Bash.Word
import           RIO hiding (Word)
import           RIO.Lens (each, _Just)
import qualified RIO.Map as M
import qualified RIO.Text as T

---

-- | A bash term which should never appear in a PKGBUILD. If one does, it's
-- either a sign of maintainer negligence or malicious behaviour.
data BannedTerm = BannedTerm Text BanCategory deriving (Eq, Ord, Show, Generic)

banCatL :: Lens' BannedTerm BanCategory
banCatL f (BannedTerm t bc) = BannedTerm t <$> f bc

-- | The reason why the bash term is black-listed.
data BanCategory = Downloading
                 | ScriptRunning
                 | Permissions
                 | InlinedBash
                 | StrangeBashism
                 | CleverRedirect
                 deriving (Eq, Ord, Show)

blacklist :: Map Text BannedTerm
blacklist = M.fromList $ downloading <> running <> permissions
  where
    downloading = map (\t -> (t, BannedTerm t Downloading)) ["curl", "wget", "rsync", "scp"]
    running     = map (\t -> (t, BannedTerm t ScriptRunning)) ["sh", "bash", "eval", "zsh", "fish"]
    permissions = map (\t -> (t, BannedTerm t Permissions)) ["sudo", "ssh"]

-- TODO wasteful conversion!
-- | Attempt to parse a PKGBUILD. Should succeed for all reasonable PKGBUILDs.
parsedPB :: Pkgbuild -> Maybe List
parsedPB (Pkgbuild pb) = hush . parse "PKGBUILD" . T.unpack $ decodeUtf8Lenient pb

-- | Discover any banned terms lurking in a parsed PKGBUILD, paired with
-- the surrounding context lines.
bannedTerms :: List -> [(ShellCommand, BannedTerm)]
bannedTerms = simpleCommands >=> bannedCommand

banned :: Word -> Maybe BannedTerm
banned w = M.lookup (T.pack $ unquote w) blacklist

-- | Extract all `SimpleCommand`s from a parsed bash AST.
simpleCommands :: List -> [ShellCommand]
simpleCommands (List ss) = ss >>= statements >>= p
  where
    p :: ShellCommand -> [ShellCommand]
    p sc@(SimpleCommand _ _) = [sc]
    p sc                     = lists sc >>= simpleCommands

    statements :: Statement -> [ShellCommand]
    statements (Statement ao _) = andor ao

    andor :: AndOr -> [ShellCommand]
    andor (Last pl)   = pipeline pl
    andor (And pl ao) = pipeline pl <> andor ao
    andor (Or pl ao)  = pipeline pl <> andor ao

    pipeline :: Pipeline -> [ShellCommand]
    pipeline (Pipeline _ _ _ cs) = map command cs

    command :: Command -> ShellCommand
    command (Command sc _) = sc

    lists :: ShellCommand -> [List]
    lists (SimpleCommand _ _) = []
    lists (AssignBuiltin _ _) = []
    lists (FunctionDef _ l)   = [l]
    lists (Coproc _ c)        = lists $ command c
    lists (Subshell l)        = [l]
    lists (Group l)           = [l]
    lists (Arith _)           = []
    lists (Cond _)            = []
    lists (For _ _ l)         = [l]
    lists (ArithFor _ l)      = [l]
    lists (Select _ _ l)      = [l]
    lists (Case _ ccs)        = map caseClause ccs
    lists (If l1 l2 ml)       = l1 : l2 : maybeToList ml
    lists (Until l1 l2)       = [l1, l2]
    lists (While l1 l2)       = [l1, l2]

    caseClause :: CaseClause -> List
    caseClause (CaseClause _ l _) = l

bannedCommand :: ShellCommand -> [(ShellCommand, BannedTerm)]
bannedCommand s@(SimpleCommand [] (g:c:_))
  | g == [Char 'g', Char 'i', Char 't'] &&
    c == [Char 'c', Char 'l', Char 'o', Char 'n', Char 'e'] = [(s, BannedTerm "git" Downloading)]
bannedCommand s@(SimpleCommand [] (c:_)) = maybeToList $ (s,) <$> banned c
bannedCommand s@(SimpleCommand as _) = as ^.. each . rValueL . to r . each
  where
    r rv@(RValue w) = maybeToList ((s,) <$> (banned w & _Just . banCatL .~ CleverRedirect)) <> q rv
    r rv = q rv

    q :: RValue -> [(ShellCommand, BannedTerm)]
    q rv = map (s,) $ join (rWords rv) >>= p

    p :: Span -> [BannedTerm]
    p (CommandSubst str)   = maybeToList (hush $ parse "CommandSubst" str) >>= simpleCommands >>= map snd . bannedCommand
    p (ArithSubst str)     = [BannedTerm (T.pack str) StrangeBashism]
    p (ProcessSubst _ str) = [BannedTerm (T.pack str) StrangeBashism]
    p sp = join (sWords sp) >>= p

    rWords :: RValue -> [Word]
    rWords (RValue w)  = [w]
    rWords (RArray ws) = ws >>= \(mw, w) -> w : maybeToList mw

    sWords :: Span -> [Word]
    sWords (Single w)      = [w]
    sWords (Double w)      = [w]
    sWords (ANSIC w)       = [w]
    sWords (Locale w)      = [w]
    sWords (Backquote w)   = [w]
    sWords (ParamSubst ps) = subWords ps
    sWords _               = []

    subWords :: ParamSubst -> [Word]
    subWords (Bare (Parameter _ mw))                = maybeToList mw
    subWords (Brace _ (Parameter _ mw))             = maybeToList mw
    subWords (Alt _ (Parameter _ mw) _ _ w)         = w : maybeToList mw
    subWords (Substring _ (Parameter _ mw) w1 w2)   = w1 : w2 : maybeToList mw
    subWords (Prefix _ _)                           = []
    subWords (Indices (Parameter _ mw))             = maybeToList mw
    subWords (Length (Parameter _ mw))              = maybeToList mw
    subWords (Delete _ (Parameter _ mw) _ _ w)      = w : maybeToList mw
    subWords (Replace _ (Parameter _ mw) _ _ w1 w2) = w1 : w2 : maybeToList mw
    subWords (LetterCase _ (Parameter _ mw) _ _ w)  = w : maybeToList mw
bannedCommand _ = []

------------
-- REPORTING
------------

-- | Dispatch different error messages depending on the category of a `BannedTerm`.
reportExploit :: BannedTerm -> (Language -> Doc AnsiStyle)
reportExploit (BannedTerm t bc) = case bc of
  Downloading    -> security_2 t
  ScriptRunning  -> security_3 t
  Permissions    -> security_4 t
  InlinedBash    -> security_8 t
  StrangeBashism -> security_9 t
  CleverRedirect -> security_10 t

--------
-- UTILS
--------

rValueL :: Lens' Assign RValue
rValueL f (Assign p ao r) = Assign p ao <$> f r
