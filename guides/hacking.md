Hacking Aura
============

Hi. You're either reading this because you want to make Aura better,
or because you want to study some Haskell. Great! 

### For Haskell Study
Aura code has examples of:
- Monad Transformers   => Aura/Monad/Aura.hs
- Parsec               => Bash/Parser.hs
- Applicative Functors => Bash/Parser.hs
- Regular Expressions  => Aura/Utils.hs
- CLI flag handling    => Aura/Flags.hs
- Shell escape codes   => Aura/Colour/Text.hs or Shell.hs

### For Aura Hacking
The `main` function is housed in `aura.hs`. All function dispatches occur here.
General libraries also housed in the root folder:
- `Bash/` (A custom Bash script parser and simplifier)
- `Data/Algorithm/Diff` (A classic diff algorithm)
- `Network/HTTP` (A copy of key functions from Network.HTTP)

Aura specific libraries are housed in `Aura/`. The main areas are:
- `Aura/`          (General libraries)
- `Aura/Commands/` (Functions that back the main capital letter Aura operations)
- `Aura/Monad/`    (Everything to do with the Aura Monad)
- `Aura/Packages/` (Backends for handling various package types)
- `Aura/Pkgbuild/` (Functions for handling PKGBUILDs)
- `Aura/Settings/` (`Settings` for the ReaderT portion of the Aura Monad)

### The Aura Monad
Many functions in the Aura code are within the Aura Monad.
The Aura Monad is in essence a glorified IO Monad.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.Error

import Aura.Settings.Base (Settings)

---

newtype Aura a = A { runA :: ErrorT AuraError (ReaderT Settings IO) a }
  deriving (Monad, MonadError AuraError, MonadReader Settings, MonadIO, Functor)

runAura :: Aura a -> Settings -> IO (Either AuraError a)
runAura a = runReaderT $ runErrorT (runA a)
```

It is an `ErrorT` at the top, meaning its binding (>>=) behaviour is the same
as an `Error` Monad. This allows failure to halt actions partway through.

It is a `ReaderT`, meaning we can obtain the local runtime settings by
using the `ask` function anywhere we wish in a function in the Aura Monad.

It is `IO` at its base, meaning we can perform IO actions with `liftIO` in
any function in the Aura Monad. To extract it's inner value, we use
the helper function `runAura`. 
