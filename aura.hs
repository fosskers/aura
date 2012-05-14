-- AURA package manager for Arch Linux

-- System libraries
import System.Environment (getArgs)
import System.Console.GetOpt

-- Custom libraries
import Pacman

data Flag = AURInstall deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['A'] ["aursync"] (NoArg AURInstall) aDesc
          ]
    where aDesc = "Install a package from the AUR."

usageMsg :: String
usageMsg = "Usage : aura <operation> [...]"  -- Intercept pacman -h?

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

main = do
  args <- getArgs
  opts <- parseOpts args
  executeOpts opts

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute options args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

executeOpts :: ([Flag],[String],[String]) -> IO ()
executeOpts (flags,input,pacOpts) =
    case flags of
      [AURInstall] -> putStrLn "This option isn't ready yet."
      _            -> (pacman $ pacOpts ++ input) >> return ()