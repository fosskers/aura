{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TypeApplications #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Core where

import           Aura.Colour
import           Aura.Languages
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Versions (prettyV)
import           Shelly (Sh, test_f)
import           Utilities

---

--------
-- TYPES
--------

-- | A `Repository` is a place where packages may be fetched from. Multiple
-- repositories can be combined with the `Semigroup` instance.
-- Check packages in batches for efficiency.
newtype Repository = Repository { repoLookup :: Settings -> S.Set T.Text -> IO (S.Set T.Text, [Package]) }

instance Semigroup Repository where
  a <> b = Repository $ \ss ps -> do
    (bads, goods)   <- repoLookup a ss ps
    (bads', goods') <- repoLookup b ss bads
    pure (bads', goods <> goods')

---------------------------------
-- Functions common to `Package`s
---------------------------------
-- | Partition a list of packages into pacman and buildable groups.
partitionPkgs :: [Package] -> ([T.Text], [Buildable])
partitionPkgs = partitionEithers . fmap (toEither . pkgInstallTypeOf)
  where toEither (Pacman s) = Left  s
        toEither (Build  b) = Right b

-----------
-- THE WORK
-----------
-- TODO Should this go somewhere else?
rethrow :: (Member (Error a) r, Member IO r) => IO (Either a b) -> Eff r b
rethrow = send >=> either throwError pure

-- | Action won't be allowed unless user is root, or using sudo.
sudo :: (Member (Reader Settings) r, Member (Error Failure) r) => Eff r a -> Eff r a
sudo action = asks (hasRootPriv . envOf) >>= bool (throwError $ Failure mustBeRoot_1) action

-- | Stop the user if they are the true Root. Building as isn't allowed
-- as of makepkg v4.2.
trueRoot :: (Member (Reader Settings) r, Member (Error Failure) r) => Eff r a -> Eff r a
trueRoot action = ask >>= \ss ->
  -- TODO Should be `(&&)` here? Double-check.
  if not (isTrueRoot $ envOf ss) || buildUserOf (buildConfigOf ss) /= Just (User "root")
    then action else throwError $ Failure trueRoot_3

-- | A list of non-prebuilt packages installed on the system.
-- `-Qm` yields a list of sorted values.
foreignPackages :: IO (S.Set SimplePkg)
foreignPackages = S.fromList . mapMaybe simplepkg' . T.lines <$> pacmanOutput ["-Qm"]

orphans :: IO [T.Text]
orphans = T.lines <$> pacmanOutput ["-Qqdt"]

develPkgs :: IO (S.Set T.Text)
develPkgs = S.filter isDevelPkg . S.map _spName <$> foreignPackages
  where isDevelPkg pkg = any (`T.isSuffixOf` pkg) suffixes
        suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- | Returns what it was given if the package is already installed.
-- Reasoning: Using raw bools can be less expressive.
isInstalled :: T.Text -> IO (Maybe T.Text)
isInstalled pkg = bool Nothing (Just pkg) <$> pacmanSuccess ["-Qq", pkg]

removePkgs :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => [T.Text] -> Eff r ()
removePkgs []   = pure ()
removePkgs pkgs = do
  pacOpts <- asks (asFlag . commonConfigOf)
  rethrow . pacman $ ["-Rsu"] <> pkgs <> pacOpts

-- | True if a dependency is satisfied by an installed package.
-- `asT` renders the `VersionDemand` into the specific form that `pacman -T`
-- understands. See `man pacman` for more info.
isSatisfied :: Dep -> IO Bool
isSatisfied (Dep name ver) = T.null <$> pacmanOutput ["-T", name <> asT ver]
  where asT (LessThan v) = "<"  <> prettyV v
        asT (AtLeast  v) = ">=" <> prettyV v
        asT (MoreThan v) = ">"  <> prettyV v
        asT (MustBe   v) = "="  <> prettyV v
        asT Anything     = ""

-- | Block further action until the database is free.
checkDBLock :: Settings -> Sh ()
checkDBLock ss = do
  locked <- test_f lockFile
  when locked $ (liftIO . warn ss . checkDBLock_1 $ langOf ss) *> liftIO getLine *> checkDBLock ss

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------

notify :: Settings -> Doc AnsiStyle -> IO ()
notify ss = putStrLnA ss . green

warn :: Settings -> Doc AnsiStyle -> IO ()
warn ss = putStrLnA ss . yellow

scold :: Settings -> Doc AnsiStyle -> IO ()
scold ss = putStrLnA ss . red

-- | Report a message with multiple associated items. Usually a list of
-- naughty packages.
report :: (Member (Reader Settings) r, Member IO r) =>
  (Doc AnsiStyle -> Doc AnsiStyle) -> (Language -> Doc AnsiStyle) -> [T.Text] -> Eff r ()
report _ _ []     = pure ()
report c msg pkgs = do
  ss <- ask
  send . putStrLnA ss . c . msg $ langOf ss
  send . T.putStrLn . dtot . colourCheck ss . vsep $ map (cyan . pretty) pkgs
