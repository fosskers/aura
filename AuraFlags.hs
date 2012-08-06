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

type OptParts = ([Char],[String],Flag,(Language -> String))

allFlags :: Language -> [OptDescr Flag]
allFlags lang = concat $ map (\f -> f lang) [ auraOperations
                                            , auraOptions
                                            , pacmanOptions
                                            , dualOptions ]

makeOption :: Language -> OptParts -> OptDescr Flag
makeOption lang (c,s,f,desc) = Option c s (NoArg f) $ desc lang

auraOperations :: Language -> [OptDescr Flag]
auraOperations lang = map (\optParts -> makeOption lang optParts)
                      [ ( ['A'], ["aursync"],   AURInstall, aurSy )
                      , ( ['C'], ["downgrade"], Cache,      downG )
                      , ( ['L'], ["logfile"],   LogFile,    logFi ) ]

auraOptions :: Language -> [OptDescr Flag]
auraOptions lang = map (\optParts -> makeOption lang optParts)
                   [ ( ['a'], ["delmakedeps"],  DelMDeps,    delma )
                   , ( ['d'], ["deps"],         ViewDeps,    viewD )
                   , ( ['p'], ["pkgbuild"],     GetPkgbuild, pkgbu )
                   , ( ['u'], ["sysupgrade"],   Upgrade,     sysup )
                   , ( ['w'], ["downloadonly"], Download,    downl )
                   , ( ['x'], ["unsuppress"],   Unsuppress,  unsup )
                   , ( [],    ["hotedit"],      HotEdit,     hotEd )
                   , ( ['c'], ["clean"],        Clean,       clean )
                   , ( ['b'], ["backup"],       Backup,      backu )
                   , ( ['s'], ["search"],       Search,      searc )
                   , ( ['i'], ["info"],         Info,        infos )
                   , ( [],    ["orphans"],      Orphans,     orpha )
                   , ( [],    ["adopt"],        Adopt,       adopt )
                   , ( [],    ["abandon"],      Abandon,     aband )
                   , ( [],    ["conf"],         ViewConf,    vConf ) 
                   , ( [],    ["languages"],    Languages,   langu ) ]

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: Language -> [OptDescr Flag]
pacmanOptions lang = map (\optParts -> makeOption lang optParts)
                     [ ( ['y'], ["refresh"], Refresh, blank )
                     , ( ['V'], ["version"], Version, blank )
                     , ( ['h'], ["help"],    Help,    blank ) ]
    where blank _ = ""

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: Language -> [OptDescr Flag]
dualOptions lang = map (\optParts -> makeOption lang optParts)
                   [ ( [], ["noconfirm"], NoConfirm, noCon ) ]

languageOptions :: [OptDescr Flag]
languageOptions = [ Option [] ["japanese"] (NoArg JapOut) "" ]

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

makeUsageMsg :: String -> [OptDescr Flag] -> String
makeUsageMsg msg flags = usageInfo (yellow msg) flags

auraOperMsg :: Language -> String
auraOperMsg lang = makeUsageMsg "Aura only operations:" $ auraOperations lang

auraOptMsg :: Language -> String
auraOptMsg lang = makeUsageMsg "Subordinate options:" $ auraOptions lang

dualFlagMsg :: Language -> String
dualFlagMsg lang = makeUsageMsg "Dual functionality options:" $ dualOptions lang

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
