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
- There is no longer a `-M` option. All ABS package interaction is done through
  `-S`.
- Installs prebuilt binaries available from Arch servers by default.
- Build options:
..* If the user specifies `--build`, the package will be built manually via
    the ABS.
..* If the user specifies `--customizepkg`, the `customizepkg` program is
    installed, and there is a specification file with the same name as the
    package being built in `/etc/customizepkg.d/`, it will be built **manually**
    via ABS.

#### AUR Package Building/Installation
- Builds manually by default, as there is no prebuilt alternative for the AUR
  (by design).

#### Dependency Resolution

#### Dependency Information Output
- Information for all immediate dependencies for any given package can be output
  in human-readable format by default with `-{A,S}d`.
- Adding `--recursive` will yield all dependencies and _their_ dependencies
  as well.
- Adding `--json` will output this information in JSON for use by other
  software that may sit on top of Aura.

#### PKGBUILD/Additional Build-file Editing
- Support for `customizepkg` is dropped, as AUR 3.0 provides dependency
  information via its API.
- Support for `--hotedit` dropped. As dependency checking is done through
  the API and not the PKGBUILDs, editing PKGBUILDs could cause a
  discrepancy between `aura` and `makepkg`. If you need fine-grained control
  over how the package is being built, building it manually with `makepkg` is
  recommended. If you're trying to fix a broken package, please contact the
  maintainer.

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

#### Other Libraries
Information on other Hackage libraries used in Aura can be found
[here](https://github.com/fosskers/aura/issues/223).

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
