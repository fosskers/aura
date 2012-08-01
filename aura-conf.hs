-- This should be at ~/.config/aura/aura-conf.hs
-- How do I get it there? And what about `.pacnew`s?

-- Do you want to always use a language other than English?
-- Specify it here as a String. Current choices:
-- English, Japanese
defaultLanguage :: String
defaultLanguage = "English"

-- These are regular expressions that can be used to protect certain
-- package files in the cache from deletion via `-Cc`. Any file that matches
-- one of the regexes in this list will never be deleted. 
-- Examples:
--  Adding "ruby" to the list below would protect packages like:
--    ruby, ruby-curb, ruby-gstreamer, ruby-json
--  Adding "^linux-3.2" would protect _all_ kernels of the 3.2 series.
-- Try using `-Cs` to figure out what regexes yield what package files.
protectedPkgs :: [String]
protectedPkgs = []

-- Do you want to always show makepkg output when building?
-- This is not default behavior, but can be triggered with `-x`.
-- Set to `True` to activate.
alwaysShowMakepkgOutput :: Bool
alwaysShowMakepkgOutput = False

-- Do you want to always prompt for pre-build PKGBUILD editing?
-- This is not default behavior, but can be triggered with `--hotedit`.
-- Set to `True` to activate.
alwaysHotEdit :: Bool
alwaysHotEdit = False
