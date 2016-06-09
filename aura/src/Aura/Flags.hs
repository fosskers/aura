{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Flags
    ( parseLanguageFlag
    , parseFlags
    , settingsFlags
    , reconvertFlags
    , dualFlagMap
    , hijackedFlagMap
    , pacmanFlagMap
    , buildABSDepsStatus
    , confirmationStatus
    , customizepkgStatus
    , delMakeDepsStatus
    , hotEditStatus
    , keepSourceStatus
    , neededStatus
    , noPowerPillStatus
    , pbDiffStatus
    , quietStatus
    , rebuildDevelStatus
    , sortSchemeStatus
    , suppressionStatus
    , truncationStatus
    , dryRunStatus
    , notSettingsFlag
    , ignoredAuraPkgs
    , makepkgFlags
    , buildPath
    , buildUser
    , auraOperMsg
    , Flag(..) ) where

import BasicPrelude hiding (FilePath, empty)

import Options.Applicative
import Options.Applicative.Internal
import Options.Applicative.Types
import qualified Options.Applicative.Help.Core as H
import qualified Options.Applicative.Help.Chunk as H
import qualified Options.Applicative.Help.Types as H
import qualified Data.Text as T

import Aura.Colour.Text (yellow)
import Aura.Settings.Base
import Aura.Languages

import Shelly (FilePath, fromText)

---

type FlagMap = Flag -> T.Text

data Flag = ABSInstall
          | AURInstall
          | SaveState
          | Cache
          | LogFile
          | Orphans
          | Search
          | Info
          | Refresh
          | GetPkgbuild
          | ViewDeps
          | DelMDeps
          | Upgrade
          | Download
          | Unsuppress
          | TreeSync
          | HotEdit
          | NoConfirm
          | DryRun
          | Quiet
          | AURIgnore T.Text
          | Ignore T.Text
          | IgnoreGroup T.Text
          | BuildPath FilePath
          | BuildUser T.Text
          | ABCSort
          | TruncHead Int
          | TruncTail Int
          | DiffPkgbuilds
          | Devel
          | Customizepkg
          | KeepSource
          | BuildABSDeps
          | Debug
          | CacheBackup
          | Clean
          | Abandon
          | ViewConf
          | RestoreState
          | NoPowerPill
          | IgnoreArch
          | Needed
          | Languages
          | Version
          | Help
          | JapOut
          | PolishOut
          | CroatianOut
          | SwedishOut
          | GermanOut
          | SpanishOut
          | PortuOut
          | FrenchOut
          | RussianOut
          | ItalianOut
          | SerbianOut
          | NorwegiOut
          | PacmanArg T.Text T.Text
            deriving (Eq, Ord, Show)

allFlags :: Language -> Parser ([Flag], [String])
allFlags lang = (,) <$> (join <$> sequenceA [ auraOperations lang
                                            , languageOptions
                                            , many $ choose $ auraOptions
                                              <> pacmanOptions
                                              <> dualOptions
                                              <> longPacmanOptions ])
  <*> many (argument str (help ""))

simpleOption :: String -> [String] -> Flag -> String -> Parser Flag
simpleOption c s f h = flag' f (help h <> foldl appendLong mempty s <> foldl appendShort mempty c)
  where appendLong a l = a <> long l
        appendShort a sh = a <> short sh

simpleOptionWarg :: String -> [String] -> (String -> Flag) -> String -> Parser Flag
simpleOptionWarg c s f h = f <$> strOption (help h <> foldl appendLong mempty s <> foldl appendShort mempty c)
  where appendLong a l = a <> long l
        appendShort a sh = a <> short sh

choose :: Alternative f => [f a] -> f a
choose = foldl (<|>) empty

auraOperations :: Language -> Parser [Flag]
auraOperations lang = choose
    [ subcommand "A" ["aursync"]   AURInstall (T.unpack $ aurSy lang)
      <*> many (choose [ simpleOption "u" ["sysupgrade"]   Upgrade       "upgrade AUR packages"
                       , simpleOption "i" ["info"]         Info          "get package info"
                       , simpleOption "s" ["search"]       Search        "Search the AUR using a Regexp"
                       , simpleOption "p" ["pkgbuild"]     GetPkgbuild   "Display an AUR Package's PKGBUILD"
                       , simpleOption "d" ["deps"]         ViewDeps      "Display an AUR package's dependencies"
                       , simpleOption "x" ["unsuppress"]   Unsuppress    "Don't supress makepkg's output"
                       , simpleOption "a" ["delmakedeps"]  DelMDeps      "Remove make depends after installing"
                       , simpleOption "k" ["diff"]         DiffPkgbuilds "Show PKGBUILD differences"
                       ])
    , subcommand "B" ["save"]      SaveState  (T.unpack $ saveS lang)
      <*> many (choose [ simpleOption  "r" ["restore"] RestoreState "Restore a saved record. Rolls back, uninstalls, and reinstalls packages as necessary"])
    , subcommand "C" ["downgrade"] Cache      (T.unpack $ downG lang)
      <*> many (choose [ simpleOption "s" ["search"] Search      "Search the package cache for package files via a regex"
                       , simpleOption "b" ["backup"] CacheBackup "Backup the package cache"
                       , simpleOption "c" ["clean"]  Clean       "Reduce the package cache to contain only 'x' of each package file"
                       ])
    , subcommand "L" ["viewlog"]   LogFile    (T.unpack $ viewL lang)
      <*> some (choose [ simpleOption "i" ["info"]   Info   "Display install / upgrade history for a package"
                       , simpleOption "s" ["search"] Search "Search the pacman logfile via a regex"
                       ])
    , subcommand "M" ["abssync"]   ABSInstall (T.unpack $ absSy lang)
      <*> many (choose [ simpleOption []  ["absdeps"]  BuildABSDeps "Build a repository package and all its dependencies manually"
                       , simpleOption "t" ["treesync"] TreeSync     "Sync a single package's data to the local ABS Tree"
                       , simpleOption "y" ["refresh"]  Refresh      "Sync all package data in the local ABS Tree"
                       ])
    , subcommand "O" ["orphans"]   Orphans    (T.unpack $ orpha lang)
      <*> many (choose [simpleOption "j" ["abandon"] Abandon "Uninstall all orphan packages"
                       ])
    , subcommand "V" ["version"] Version ""
      <*> pure []
    ]
  where subcommand a b c d = (\a b -> [a] <> b) <$> simpleOption a b c d

auraOptions :: [Parser Flag]
auraOptions =
  [ simpleOptionWarg []  ["aurignore"] (AURIgnore . T.pack) ""
  , simpleOptionWarg []  ["build"]     (BuildPath . fromText . T.pack) ""
  , simpleOptionWarg []  ["builduser"] (BuildUser . T.pack) ""
  , simpleOptionWarg []  ["head"]      (TruncHead . read . T.pack) ""
  , simpleOptionWarg []  ["tail"]      (TruncTail . read . T.pack) ""
  , simpleOption "w" ["downloadonly"] Download     ""
  , simpleOption "u" ["sysupgrade"]   Upgrade      ""
  , simpleOption "q" ["quiet"]        Quiet        ""
  , simpleOption []  ["abc"]          ABCSort      ""
  , simpleOption []  ["allsource"]    KeepSource   ""
  , simpleOption []  ["auradebug"]    Debug        ""
  , simpleOption []  ["custom"]       Customizepkg ""
  , simpleOption []  ["devel"]        Devel        ""
  , simpleOption []  ["hotedit"]      HotEdit      ""
  , simpleOption []  ["ignorearch"]   IgnoreArch   ""
  , simpleOption []  ["languages"]    Languages    ""
  , simpleOption []  ["no-pp"]        NoPowerPill  ""
  , simpleOption []  ["dryrun"]       DryRun       ""
  , simpleOption []  ["viewconf"]     ViewConf     "" ]

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: [Parser Flag]
pacmanOptions =
  [ simpleOption "y" ["refresh"] Refresh "Sync all package data in the local ABS Tree"
  , simpleOption "V" ["version"] Version "display version information"
  , simpleOption "h" ["help"]    Help    "display help"
  ]

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: [Parser Flag]
dualOptions =
  [ simpleOptionWarg [] ["ignore"]      (Ignore . T.pack)  ""
  , simpleOptionWarg [] ["ignoregroup"] (IgnoreGroup . T.pack) ""
  , simpleOption     [] ["noconfirm"] NoConfirm ""
  , simpleOption     [] ["needed"]    Needed   ""
  ]

-- These Pacman options are ignored,
-- but parser needs to know that they require an argument
longPacmanOptions :: [Parser Flag]
longPacmanOptions =  fmap pacArg $ zip
  [ "dbpath", "root", "arch", "cachedir", "color"
  , "config", "gpgdir" , "logfile", "assume-installed"
  , "print-format" ]
  ( "b" : "r" : repeat [] )
  -- "owns" is apparently okay as is?
  -- TODO: check all others
  where pacArg (opt, letter) = (PacmanArg (T.pack opt) . T.pack) <$> strOption (long opt <> foldl appendShort mempty letter)
        appendShort a s = a <> short s

pacmanFlagMap :: FlagMap
pacmanFlagMap (PacmanArg option arg) = "--" <> option <> "=" <> arg
pacmanFlagMap _                      = ""

languageOptions :: Parser [Flag]
languageOptions = fmap maybeToList $ optional $ choose $ fmap (\(a,b,c,d) -> simpleOption a b c d)
                  [ ( [], ["japanese", "日本語"],      JapOut      , "")
                  , ( [], ["polish", "polski"],        PolishOut   , "")
                  , ( [], ["croatian", "hrvatski"],    CroatianOut , "")
                  , ( [], ["swedish", "svenska"],      SwedishOut  , "")
                  , ( [], ["german", "deutsch"],       GermanOut   , "")
                  , ( [], ["spanish", "español"],      SpanishOut  , "")
                  , ( [], ["portuguese", "português"], PortuOut    , "")
                  , ( [], ["french", "français"],      FrenchOut   , "")
                  , ( [], ["russian", "русский"],      RussianOut  , "")
                  , ( [], ["italian", "italiano"],     ItalianOut  , "")
                  , ( [], ["serbian", "српски"],       SerbianOut  , "")
                  , ( [], ["norwegian", "norsk"],      NorwegiOut  , "") ]

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = simpleFlagMap [ (CacheBackup,   "-b" )
                                , (Clean,         "-c" )
                                , (ViewDeps,      "-d" )
                                , (Info,          "-i" )
                                , (DiffPkgbuilds, "-k" )
                                , (RestoreState,  "-r" )
                                , (Search,        "-s" )
                                , (TreeSync,      "-t" )
                                , (Upgrade,       "-u" )
                                , (Download,      "-w" )
                                , (Refresh,       "-y" ) ]

-- These are flags which do the same thing in Aura or Pacman.
dualFlagMap :: FlagMap
dualFlagMap (Ignore      a) = "--ignore="      <> a
dualFlagMap (IgnoreGroup a) = "--ignoregroup=" <> a
dualFlagMap f = flip simpleFlagMap f [ (Quiet,     "-q"          )
                                     , (NoConfirm, "--noconfirm" )
                                     , (Needed,    "--needed"    ) ]

simpleFlagMap :: [(Flag, T.Text)] -> Flag -> T.Text
simpleFlagMap fm = fromMaybe "" . flip lookup fm

-- Converts the intercepted Pacman flags back into their raw string forms
-- and filters out the garbage.
reconvertFlags :: FlagMap -> [Flag] -> [T.Text]
reconvertFlags fm = filter (not . T.null) . fmap fm

settingsFlags :: [Flag]
settingsFlags = [ Unsuppress, NoConfirm, HotEdit, DiffPkgbuilds, Debug, Devel
                , DelMDeps, Customizepkg, Quiet, NoPowerPill, KeepSource
                , BuildABSDeps, ABCSort, IgnoreArch, DryRun, Needed ]

-- Flags like `AURIgnore` and `BuildPath` have args, and thus can't be included
-- in the `settingsFlags` list.
notSettingsFlag :: Flag -> Bool
notSettingsFlag (AURIgnore _)   = False
notSettingsFlag (BuildPath _)   = False
notSettingsFlag (BuildUser _)   = False
notSettingsFlag (TruncHead _)   = False
notSettingsFlag (TruncTail _)   = False
notSettingsFlag (PacmanArg _ _) = False
notSettingsFlag f               = f `notElem` settingsFlags

auraOperMsg :: Language -> T.Text
auraOperMsg lang = T.pack $ H.renderHelp 100
  $  H.usageHelp (H.cmdDesc (auraOperations lang))
  <> H.headerHelp (H.stringChunk $ T.unpack $ yellow $ auraOperTitle lang)

-- Extracts desirable results from given Flags.
-- Callers must supply an [alt]ernate value for when there are no matches.
fishOutFlag :: [(Flag, a)] -> a -> [Flag] -> a
fishOutFlag [] alt _             = alt
fishOutFlag ((f, r):fs) alt flags | f `elem` flags = r
                                 | otherwise      = fishOutFlag fs alt flags

getLanguage :: [Flag] -> Maybe Language
getLanguage = fishOutFlag flagsAndResults Nothing
    where flagsAndResults = zip langFlags langFuns
          langFlags       = [ JapOut, PolishOut, CroatianOut, SwedishOut
                            , GermanOut, SpanishOut, PortuOut, FrenchOut
                            , RussianOut, ItalianOut, SerbianOut, NorwegiOut ]
          langFuns        = Just <$> [Japanese ..]

ignoredAuraPkgs :: [Flag] -> [T.Text]
ignoredAuraPkgs [] = []
ignoredAuraPkgs (AURIgnore ps : _) = T.splitOn "," ps
ignoredAuraPkgs (_:fs) = ignoredAuraPkgs fs

buildPath :: [Flag] -> FilePath
buildPath [] = ""
buildPath (BuildPath p : _) = p
buildPath (_:fs) = buildPath fs

buildUser :: [Flag] -> Maybe T.Text
buildUser [] = Nothing
buildUser (BuildUser u : _) = Just u
buildUser (_:fs) = buildUser fs

truncationStatus :: [Flag] -> Truncation
truncationStatus [] = None
truncationStatus (TruncHead n : _) = Head n
truncationStatus (TruncTail n : _) = Tail n
truncationStatus (_:fs) = truncationStatus fs

sortSchemeStatus :: [Flag] -> SortScheme
sortSchemeStatus = fishOutFlag [(ABCSort, Alphabetically)] ByVote

suppressionStatus :: [Flag] -> Bool
suppressionStatus = fishOutFlag [(Unsuppress, False)] True

delMakeDepsStatus :: [Flag] -> Bool
delMakeDepsStatus = fishOutFlag [(DelMDeps, True)] False

confirmationStatus :: [Flag] -> Bool
confirmationStatus = fishOutFlag [(NoConfirm, False)] True

neededStatus :: [Flag] -> Bool
neededStatus = fishOutFlag [(Needed, True)] False

hotEditStatus :: [Flag] -> Bool
hotEditStatus = fishOutFlag [(HotEdit, True)] False

pbDiffStatus :: [Flag] -> Bool
pbDiffStatus = fishOutFlag [(DiffPkgbuilds, True)] False

quietStatus :: [Flag] -> Bool
quietStatus = fishOutFlag [(Quiet, True)] False

rebuildDevelStatus :: [Flag] -> Bool
rebuildDevelStatus = fishOutFlag [(Devel, True)] False

customizepkgStatus :: [Flag] -> Bool
customizepkgStatus = fishOutFlag [(Customizepkg, True)] False

noPowerPillStatus :: [Flag] -> Bool
noPowerPillStatus = fishOutFlag [(NoPowerPill, True)] False

keepSourceStatus :: [Flag] -> Bool
keepSourceStatus = fishOutFlag [(KeepSource, True)] False

buildABSDepsStatus :: [Flag] -> Bool
buildABSDepsStatus = fishOutFlag [(BuildABSDeps, True)] False

dryRunStatus :: [Flag] -> Bool
dryRunStatus = fishOutFlag [(DryRun, True)] False

makepkgFlags :: [Flag] -> [String]
makepkgFlags = fishOutFlag [(IgnoreArch, ["--ignorearch"])] []

parseLanguageFlag :: [String] -> Maybe Language
parseLanguageFlag args = getParseResult (execParserPure defaultPrefs opts args) >>= getLanguage
  where opts = info (languageOptions) (fullDesc)

parseFlags :: Maybe Language -> [String] -> IO ([Flag], [T.Text], [T.Text])
parseFlags lang args = parseFlags' args $ maybe English id lang

partialParserPure :: Parser a -> [String] -> ParserResult (a, [String])
partialParserPure pinfo args =
  case runP p pprefs of
    (Right  r, _) -> Success r
    (Left err, ctx) -> Failure $ parserFailure pprefs (info pinfo (briefDesc)) err ctx
  where
    pprefs = defaultPrefs
    p = runParser SkipOpts pinfo args

-- Errors are dealt with manually in `aura.hs`.
parseFlags' :: [String] -> Language -> IO ([Flag], [T.Text], [T.Text])
parseFlags' args lang = tryPacman (partialParserPure opts args)
  where flatten ((opts, nonOpts), pacOpts) = (opts, map T.pack nonOpts, map T.pack pacOpts)
        opts = helper <*> allFlags lang
        tryPacman p = case getParseResult p of
          Nothing -> (\a -> ([], [], a)) <$> getArgs
          Just r -> pure $ flatten r
