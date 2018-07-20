{-# LANGUAGE OverloadedStrings, TupleSections #-}

-- |
-- Module    : Aura.Pkgbuild.Security
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Aura.Pkgbuild.Security ( exploits ) where

import           Aura.Languages
import           Aura.Types (Pkgbuild(..), Failure(..))
import           BasePrelude hiding (Last, Word)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Language.Bash.Cond as Cond
import           Language.Bash.Parse (parse)
import           Language.Bash.Syntax
import           Language.Bash.Word (Word, unquote)
import           Lens.Micro

---

data BannedTerm = Curl | Wget | Git | Ssh deriving (Eq, Ord, Show)  -- TODO add more

blacklist :: M.Map T.Text BannedTerm
blacklist = M.fromList [("curl", Curl), ("wget", Wget), ("git", Git), ("ssh", Ssh)]

-- | Determine if a given PKGBUILD contains bash exploits.
-- Will also fail if parsing itself failed.
exploits :: Pkgbuild -> Maybe Failure
exploits (Pkgbuild pb) = case parse "PKGBUILD" $ T.unpack pb of
  Left _ -> Just $ Failure security_1
  Right (List sms) -> case mapMaybe exploit sms of
    [] -> Nothing
    es -> (show $ map fst es) `trace` (Just . Failure $ const "OUCH!")

exploit :: Statement -> Maybe (NonEmptySet BannedTerm, Statement)
exploit s = traverse banned (s ^.. terms) >>= fmap (,s) . NES.fromSet . S.fromList

banned :: Word -> Maybe BannedTerm
banned w = M.lookup (T.pack $ unquote w) blacklist

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
command = lens commands (\pl cs -> pl { commands = cs })

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
