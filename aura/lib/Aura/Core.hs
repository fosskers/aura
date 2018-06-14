{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}

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

import           Aura.Colour.Text
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings.Base
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Data.Semigroup
import qualified Data.Text as T
import           Shelly (Sh, test_f)
import           Utilities

---

--------
-- TYPES
--------

-- | A 'Repository' is a place where packages may be fetched from. Multiple
-- repositories can be combined with the 'Data.Monoid' instance.
newtype Repository = Repository { repoLookup :: forall m. MonadIO m => Settings -> T.Text -> m (Maybe Package) }

instance Semigroup Repository where
  a <> b = Repository $ \ss p -> do
    mpkg <- repoLookup a ss p
    case mpkg of
      Nothing -> repoLookup b ss p
      _       -> pure mpkg

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
-- | Action won't be allowed unless user is root, or using sudo.
sudo :: Aura a -> Aura (Either Failure a)
sudo action =
  asks (hasRootPriv . envOf) >>= bool (pure $ failure mustBeRoot_1) (Right <$> action)

-- | Stop the user if they are the true Root. Building as isn't allowed
-- as of makepkg v4.2.
trueRoot :: Aura a -> Aura (Either Failure a)
trueRoot action = ask >>= \ss ->
  -- TODO Should be `(&&)` here? Double-check.
  if not (isTrueRoot $ envOf ss) || buildUserOf (buildConfigOf ss) /= Just (User "root")
    then Right <$> action else pure $ failure trueRoot_3

-- | A list of non-prebuilt packages installed on the system.
-- `-Qm` yields a list of sorted values.
foreignPackages :: Aura [SimplePkg]
foreignPackages = mapMaybe simplepkg' . T.lines <$> pacmanOutput ["-Qm"]

orphans :: Aura [T.Text]
orphans = T.lines <$> pacmanOutput ["-Qqdt"]

develPkgs :: Aura [T.Text]
develPkgs = filter isDevelPkg . map _spName <$> foreignPackages
  where isDevelPkg pkg = any (`T.isSuffixOf` pkg) suffixes
        suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- | Returns what it was given if the package is already installed.
-- Reasoning: Using raw bools can be less expressive.
isInstalled :: MonadIO m => T.Text -> m (Maybe T.Text)
isInstalled pkg = bool Nothing (Just pkg) <$> pacmanSuccess ["-Qq", pkg]

removePkgs :: [T.Text] -> Aura (Either Failure ())
removePkgs []   = pure $ Right ()
removePkgs pkgs = do
  pacOpts <- asks (map asFlag . toList . commonOptsOf)
  pacman $ ["-Rsu"] <> pkgs <> pacOpts

-- | True if a dependency is satisfied by an installed package.
isSatisfied :: MonadIO m => Dep -> m Bool
isSatisfied (Dep name ver) = T.null <$> pacmanOutput ["-T", name <> T.pack (show ver)]

-- | Block further action until the database is free.
checkDBLock :: Settings -> Sh ()
checkDBLock ss = do
  locked <- test_f lockFile
  when locked $ (warn . checkDBLock_1 $ langOf ss) *> liftIO getLine *> checkDBLock ss

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------

renderColour :: Colouror -> (Language -> T.Text) -> Aura T.Text
renderColour c msg = asks (c . msg . langOf)

notify :: MonadIO m => T.Text -> m ()
notify = putStrLnA green

warn :: MonadIO m => T.Text -> m ()
warn = putStrLnA yellow

scold :: MonadIO m => T.Text -> m ()
scold = putStrLnA red

badReport :: (Language -> T.Text) -> [T.Text] -> Aura ()
badReport _ []     = pure ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs
