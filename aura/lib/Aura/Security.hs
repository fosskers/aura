-- |
-- Module    : Aura.Security
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Code common to the analysis and display of PKGBUILD security issues.

module Aura.Security where

import           Aura.Core
import           Aura.IO
import           Aura.Languages
import           Aura.Pkgbuild.Security
import           Aura.Settings
import           Aura.Types
import           Language.Bash.Pretty (prettyText)
import           Language.Bash.Syntax
import           RIO
import qualified RIO.Text as T

---

-- | Determine if a package's PKGBUILD might contain malicious bash code.
analysePkgbuild :: Buildable -> RIO Env ()
analysePkgbuild b = do
  ss <- asks settings
  let f = do
        yes <- liftIO $ optionalPrompt ss security_6
        when yes . throwM $ Failure security_7
  case parsedPB $ bPkgbuild b of
    Nothing -> warn ss (security_1 $ bName b) *> f
    Just l  -> case bannedTerms l of
      []  -> pure ()
      bts -> do
        scold ss . security_5 $ bName b
        liftIO $ traverse_ (displayBannedTerms ss) bts
        f

displayBannedTerms :: Settings -> (ShellCommand, BannedTerm) -> IO ()
displayBannedTerms ss (stmt, b) = do
  putTextLn . T.pack $ "\n    " <> prettyText stmt <> "\n"
  warn ss $ reportExploit b
