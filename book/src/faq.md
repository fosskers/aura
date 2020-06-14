# FAQ

## Why doesn't package XYZ build?

If you're on the most recent version of Aura and are still having problems, this
could be for a few reasons:

- There is something wrong with the upstream package source files or a
  dependency.
  - If so, check the comments on the AUR page for that package.
- There is something strange going on with environment variables.
  - If so, double-check your `aura.conf` and LOCALE.
- You've found a real bug in Aura.
  - [Please report it!](https://github.com/fosskers/aura/issues)

## How do I debug a problem?

To display extra output during Aura's operation, pass `--log-level=debug`. If
Aura is failing mysteriously somewhere, that output will at least let us know
how far it got internally.
