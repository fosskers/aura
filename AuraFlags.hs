module AuraFlags where

import System.Console.GetOpt

import AuraLanguages
import Utilities

type FlagMap = [(Flag,String)]

data Flag = AURInstall | Cache | GetPkgbuild | Search | Refresh |
            Upgrade | Download | Unsuppress | NoConfirm | Languages |
            Version | Help | JapOut deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]      (NoArg AURInstall)  aDesc
              , Option ['u'] ["sysupgrade"]   (NoArg Upgrade)     uDesc
              , Option ['w'] ["downloadonly"] (NoArg Download)    wDesc
              , Option ['p'] ["pkgbuild"]     (NoArg GetPkgbuild) pDesc
              , Option ['x'] ["unsuppress"]   (NoArg Unsuppress)  xDesc
              , Option ['C'] ["downgrade"]    (NoArg Cache)       cDesc
              , Option ['s'] ["search"]       (NoArg Search)      sDesc
              ]
    where aDesc = "Install from the [A]UR."
          uDesc = "(With -A) Upgrade all installed AUR packages."
          wDesc = "(With -A) Download the source tarball only."
          pDesc = "(With -A) Output the contents of a package's PKGBUILD."
          xDesc = "(With -A) Unsuppress makepkg output."
          cDesc = "Perform actions involving the package [C]ache.\n" ++
                  "Default action downgrades given packages."
          sDesc = "(With -C) Search the package cache via a regex pattern."

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

-- Converts an intercepted Pacman flag back into its raw string form.
reconvertFlag :: FlagMap -> Flag -> String
reconvertFlag flagMap f = case f `lookup` flagMap of
                            Just x  -> x
                            Nothing -> ""

settingsFlags :: [Flag]
settingsFlags = [Unsuppress,NoConfirm,JapOut]

allFlags :: [OptDescr Flag]
allFlags = auraOptions ++ pacmanOptions ++ dualOptions ++ languageOptions

makeUsageMsg :: Colour -> String -> [OptDescr Flag] -> String
makeUsageMsg c msg flags = usageInfo (colourize c msg) flags

auraUsageMsg :: String
auraUsageMsg = makeUsageMsg yellow "AURA only operations:" auraOptions

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
