module AuraFlags where

-- System Libraries
import System.Console.GetOpt

-- Custom Libraries
import Utilities (notNull)
import Shell (yellow)
import AuraLanguages

type FlagMap = [(Flag,String)]

data Flag = AURInstall
          | Cache
          | LogFile
          | Search
          | Info
          | Refresh
          | GetPkgbuild
          | ViewDeps
          | DelMDeps
          | Upgrade
          | Download
          | Unsuppress
          | HotEdit
          | NoConfirm
          | Backup
          | Clean
          | Orphans
          | Adopt
          | Abandon
          | ViewConf
          | Languages
          | Version
          | Help
          | JapOut
            deriving (Eq,Ord)

type OptParts = ([Char],[String],Flag,String)

allFlags :: Language -> [OptDescr Flag]
allFlags lang = concat [ auraOperations lang
                       , auraOptions
                       , pacmanOptions
                       , dualOptions ]

makeOption :: OptParts -> OptDescr Flag
makeOption (c,s,f,desc) = Option c s (NoArg f) desc

auraOperations :: Language -> [OptDescr Flag]
auraOperations lang = map makeOption
                      [ ( ['A'], ["aursync"],   AURInstall, aurSy lang )
                      , ( ['C'], ["downgrade"], Cache,      downG lang )
                      , ( ['L'], ["viewlog"],   LogFile,    viewL lang ) ]

auraOptions :: [OptDescr Flag]
auraOptions = map makeOption
              [ ( ['a'], ["delmakedeps"],  DelMDeps,    "" )
              , ( ['d'], ["deps"],         ViewDeps,    "" )
              , ( ['p'], ["pkgbuild"],     GetPkgbuild, "" )
              , ( ['u'], ["sysupgrade"],   Upgrade,     "" )
              , ( ['w'], ["downloadonly"], Download,    "" )
              , ( ['x'], ["unsuppress"],   Unsuppress,  "" )
              , ( [],    ["hotedit"],      HotEdit,     "" )
              , ( ['c'], ["clean"],        Clean,       "" )
              , ( ['b'], ["backup"],       Backup,      "" )
              , ( ['s'], ["search"],       Search,      "" )
              , ( ['i'], ["info"],         Info,        "" )
              , ( [],    ["orphans"],      Orphans,     "" )
              , ( [],    ["adopt"],        Adopt,       "" )
              , ( [],    ["abandon"],      Abandon,     "" )
              , ( [],    ["conf"],         ViewConf,    "" ) 
              , ( [],    ["languages"],    Languages,   "" ) ]

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = map makeOption
                [ ( ['y'], ["refresh"], Refresh, "" )
                , ( ['V'], ["version"], Version, "" )
                , ( ['h'], ["help"],    Help,    "" ) ]

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: [OptDescr Flag]
dualOptions = map makeOption
              [ ( [], ["noconfirm"], NoConfirm, "" ) ]

languageOptions :: [OptDescr Flag]
languageOptions = map makeOption
                  [ ( [], ["japanese","nihongo"], JapOut, "" ) ]

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = [ (Backup,"-b")
                  , (Clean,"-c")
                  , (ViewDeps,"-d")
                  , (Info,"-i")
                  , (Search,"-s")
                  , (Upgrade,"-u")
                  , (Download,"-w")
                  , (Refresh,"-y") ]

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
settingsFlags = [Unsuppress,NoConfirm,HotEdit,JapOut]

auraOperMsg :: Language -> String
auraOperMsg lang = usageInfo (yellow $ auraOperTitle lang) $ auraOperations lang

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

getHotEdit :: [Flag] -> Bool
getHotEdit = fishOutFlag [(HotEdit,True)] False

parseLanguageFlag :: [String] -> (Language,[String])
parseLanguageFlag args =
    case getOpt' Permute languageOptions args of
      (langs,nonOpts,otherOpts,_) -> (getLanguage langs, nonOpts ++ otherOpts)

-- Errors are dealt with manually in `aura.hs`.
parseOpts :: Language -> [String] -> ([Flag],[String],[String])
parseOpts lang args = case getOpt' Permute (allFlags lang) args of
                        (opts,nonOpts,pacOpts,_) -> (opts,nonOpts,pacOpts) 
