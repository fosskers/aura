# Aura 2 Design

## Contents
Links to sections here. Is this possible in Markdown?

## Preface
This is a design document for version 2 of
[Aura](https://github.com/fosskers/aura). Note that specifications are written
in present tense, as in, "Aura does this" even if at the time of writing those
features aren't implemented yet. This is to ensure that the document can act
as a reference for Aura's behaviour post-release.

## Requirements
### Functional Requirements

#### ABS Package Building/Installation
- Installs prebuilt binaries available from Arch servers by default.
  If the user specifies `--build`, the package will be built manually via
  the ABS.

#### AUR Package Building/Installation
- Builds manually by default, as there is no prebuilt alternative for the AUR.

#### Dependency Resolution

#### Dependency Information Output
- Information for all immediate dependencies for any given package can be output
  in human-readable format by default with `-{A,M}d`.
- Adding `--json` will output this information in JSON for use by other
  softare that may sit on top of Aura.

#### PKGBUILD/.install File Editing
- If the user specifies `--edit` when building, they will be prompted to edit
  PKGBUILDs and .install files (if they are present after download) in the
  editor specifid by their local user's (non-sudo) $EDITOR variable.

### Haskell Requirements
#### Strings
- All Strings are represented as `Text` from `Data.Text`. This is available
  in the `text` package from Hackage.

```haskell
{-# LANGUAGE OverloadedStrings #-}
```
should be used where appropriate for String literals being converted to Text
automatically.

#### JSON Data
- All JSON input and output is handled through `aeson` and
  `aeson-pretty`.

### Package Requirements
Aura must be available in the following forms:
- `haskell-aura` An AUR package pulled from Hackage, with all special install
  instructions contained in `Setup.hs`.
- `aura` What was `aura-bin` in Aura 1. A pre-built binary for those with
  no interest in Haskell. The old `aura-bin` package will be noted as 
  depreciated, left as Aura 1, and removed from the AUR **two** months after
  the release of Aura 2.
- `aura-git` the same as is currently available. Should man page install
  instructions, etc., be in `Setup.hs` the same as `haskell-aura`?
