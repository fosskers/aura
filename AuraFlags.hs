module AuraFlags where

import System.Console.GetOpt

import AuraLanguages
import Utilities

type FlagMap = [(Flag,String)]

data Flag = AURInstall
          | Cache
          | GetPkgbuild
          | Search
          | Refresh
          | Upgrade
          | DelMDeps
          | Download
          | Unsuppress
          | NoConfirm
          | Languages
          | Backup
          | ViewLog
          | Orphans
          | Adopt
          | Abandon
          | Version
          | Help
          | JapOut
            deriving (Eq,Ord)

auraOperations :: [OptDescr Flag]
auraOperations = [ Option ['A'] ["aursync"]   (NoArg AURInstall) aDesc
                 , Option ['C'] ["downgrade"] (NoArg Cache)      cDesc
                 ]
    where aDesc = "Install from the [A]UR."
          cDesc = "Perform actions involving the package [C]ache.\n" ++
                  "Default action downgrades given packages."

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['a'] ["delmakedeps"]  (NoArg DelMDeps)    delma
              , Option ['p'] ["pkgbuild"]     (NoArg GetPkgbuild) pkgbu
              , Option ['u'] ["sysupgrade"]   (NoArg Upgrade)     sysup
              , Option ['w'] ["downloadonly"] (NoArg Download)    downl
              , Option ['x'] ["unsuppress"]   (NoArg Unsuppress)  unsup
              , Option ['s'] ["search"]       (NoArg Search)      searc
              , Option ['z'] ["backup"]       (NoArg Backup)      backu
              , Option []    ["log"]          (NoArg ViewLog)     log
              , Option []    ["orphans"]      (NoArg Orphans)     orpha
              , Option []    ["adopt"]        (NoArg Adopt)       adopt
              , Option []    ["abandon"]      (NoArg Abandon)     aband
              ]
    where delma = "(With -A) Remove unneeded make dependencies after install."
          sysup = "(With -A) Upgrade all installed AUR packages."
          downl = "(With -A) Download the source tarball only."
          pkgbu = "(With -A) Output the contents of a package's PKGBUILD."
          unsup = "(With -A) Unsuppress makepkg output."          
          searc = "(With -C) Search the package cache via a regex pattern."
          backu = "(With -C) Backup the package cache to a given directory."
          log   = "View the Pacman log file. (uses `more`)"
          orpha = "Display orphan packages. (No longer needed dependencies.)"
          adopt = "Adopt an orphan package. Shortcut for `-D --asexplicit`."
          aband = "Uninstall all orphan packages."

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = [ Option ['y'] ["refresh"] (NoArg Refresh) ""
                , Option ['V'] ["version"] (NoArg Version) ""
                , Option ['h'] ["help"]    (NoArg Help)    ""
                ]

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: [OptDescr Flag]
dualOptions = [ Option [] ["noconfirm"] (NoArg NoConfirm) ncDesc ]
    where ncDesc = "Never ask for any Aura or Pacman confirmation."

languageOptions :: [OptDescr Flag]
languageOptions = [ Option [] ["languages"] (NoArg Languages) lDesc
                  , Option [] ["japanese"]  (NoArg JapOut)    jDesc
                  ]
    where lDesc = "Display the available output languages for aura."
          jDesc = "All aura output is given in Japanese."

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = [ (Search,"-s"),(Refresh,"-y"),(Upgrade,"-u")
                , (Download,"-w") ]

dualFlagMap :: FlagMap
dualFlagMap = [ (NoConfirm,"--noconfirm") ]

-- Does the whole lot and filters out the garbage.
reconvertFlags :: [Flag] -> FlagMap -> [String]
reconvertFlags flags fm = filter notNull $ map (reconvertFlag fm) flags

-- Converts an intercepted Pacman flag back into its raw string form.
reconvertFlag :: FlagMap -> Flag -> String
reconvertFlag flagMap f = case f `lookup` flagMap of
                            Just x  -> x
                            Nothing -> ""

settingsFlags :: [Flag]
settingsFlags = [Unsuppress,NoConfirm,JapOut]

allFlags :: [OptDescr Flag]
allFlags = auraOperations ++ auraOptions ++ pacmanOptions ++
           dualOptions ++ languageOptions

makeUsageMsg :: Colour -> String -> [OptDescr Flag] -> String
makeUsageMsg c msg flags = usageInfo (colourize c msg) flags

auraOperMsg :: String
auraOperMsg = makeUsageMsg yellow "Aura only operations:" auraOperations

auraOptMsg :: String
auraOptMsg = makeUsageMsg yellow "Subordinate options:" auraOptions

dualFlagMsg :: String
dualFlagMsg = makeUsageMsg yellow "Dual functionality options:" dualOptions

languageMsg :: String
languageMsg = makeUsageMsg yellow "Language options:" languageOptions

-- Extracts desirable results from given Flags.
-- Callers must supply an alternate value for when there are no matches.
fishOutFlag :: [(Flag,a)] -> a -> [Flag] -> a
fishOutFlag [] alt _             = alt
fishOutFlag ((f,r):fs) alt flags | f `elem` flags = r
                                 | otherwise      = fishOutFlag fs alt flags

getLanguage :: [Flag] -> Language
getLanguage = fishOutFlag flagsAndResults english
    where flagsAndResults = zip langFlags langFuns
          langFlags       = [JapOut]
          langFuns        = [japanese]

getSuppression :: [Flag] -> Bool
getSuppression = fishOutFlag [(Unsuppress,False)] True

getConfirmation :: [Flag] -> Bool
getConfirmation = fishOutFlag [(NoConfirm,False)] True

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute allFlags args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 
