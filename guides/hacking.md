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
- `Bash/`               (A custom Bash script parser and simplifier)
- `Data/Algorithm/Diff` (A classic diff algorithm)
- `Network/HTTP`        (A copy of key functions from Network.HTTP)
- `Internet`            (For https requests)
- `Shell`               (Shell access in the IO Monad)
- `Utilities`           (Random helper functions)

Aura specific libraries are housed in `Aura/`. The main areas are:
- `Aura/`          (General Aura-specific libraries)
- `Aura/Commands/` (Functions that back the main capital letter Aura operations)
- `Aura/Monad/`    (Everything to do with the Aura Monad)
- `Aura/Packages/` (Backends for handling various package types)
- `Aura/Pkgbuild/` (Functions for handling PKGBUILDs)
- `Aura/Settings/` (`Settings` for the ReaderT portion of the Aura Monad)

### The Aura Monad
Many functions in the Aura code are within the Aura Monad.
The Aura Monad is a stack of Monad Transformers, but is
in essence a glorified IO Monad.

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

data AuraError = M String deriving (Eq,Show)

instance Error AuraError where
    noMsg  = strMsg "No error message given."
    strMsg = M
```

The Aura Monad is an `ErrorT` at the top, meaning its binding (>>=) behaviour
is the same as an `Error` Monad. This allows failure to halt actions partway
through.

It is a `ReaderT` and has a MonadReader instance, meaning we can obtain
the local runtime settings by using the `ask` function anywhere we wish
in a function within the Aura Monad.

It is `IO` at its base and has a MonadIO instance, meaning we can perform
IO actions with `liftIO` in any function in the Aura Monad.
To extract it's inner value, we use the helper function `runAura`.

#### Why the Aura Monad?
The Aura Monad is convenient for two reasons:

1. The local runtime settings are referenced heavily throughout
   the Aura code. Passing a `Settings` parameter around explicitely makes for long
   function signatures. Furthmore, being accessed from an internal Reader Monad
   also means its access is _read-only_. This way, the run-time settings
   could never be altered unknowingly.

2. Being an `ErrorT`, it can fail. These failures can also be caught elegantly,
   demanding no need for try/catch blocks a la imperitive languages. Example:

```haskell
foo :: Whatever -> Aura Whatever
foo w = risky w >>= more >>= evenMore >>= most
```

Here, if `risky` fails, `more`, `evenMore`, and `most` will never execute.
Anything binding `foo` at a higher level would also fail accordingly if not
caught.

In essence, if you've ever programmed in a language with error
handling and an idea of constant global variables, you've programmed in
the Aura Monad.

#### Notes on Aura Monad Style
Access to `Settings` is frequently needed, thus calls to `ask` are plentiful.
When writing a function in the Aura Monad with `do` notation and calling `ask`,
please do so in the following way:

```haskell
foo :: Whatever -> Aura Whatever
foo w = ask >>= \ss -> do
  ...  -- Rest of the function.
```

If you only need one function out of `Settings`, you can use `asks`,
which directly applies a function to the result of `ask`:

```haskell
-- For example, if I only need the cache path from Settings...
foo :: Whatever -> Aura Whatever
foo w = asks cachePathOf >>= \path -> do
  ...  -- Rest of the function.
```

The idea is to keep interaction with `ask` to the first line, before `do`.

---

### String Dispatching
No Strings meant for user-viewed output are hardcoded. All current translations
of all Strings are kept in `Aura/Languages.hs`. Messages are fetched by
helper functions after being passed the current runtime `Language` stored in
`Settings`. This leads to:

1. More advanced String manipulation, regardless of spoken language.

2. More convenient translation work.

3. (Unfortunately) larger executable size.

See the Localisation Guide for more information.
