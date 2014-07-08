-- Thin layer to mix `Shell` with the Aura Monad.

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

module Aura.Shell
    ( shellCmd
    , quietShellCmd
    , quietShellCmd'
    , checkExitCode
    , checkExitCode' ) where

import System.Exit (ExitCode)

import Aura.Monad.Aura

import Utilities (tripleSnd)

import qualified Shell as S

---

shellCmd :: String -> [String] -> Aura ()
shellCmd cmd args = liftIO (S.shellCmd cmd args) >>= checkExitCode

quietShellCmd :: String -> [String] -> Aura String
quietShellCmd cmd args = tripleSnd <$> liftIO (S.quietShellCmd' cmd args)

-- More verbose return type.
quietShellCmd' :: String -> [String] -> Aura (ExitCode,String,String)
quietShellCmd' cmd args = liftIO $ S.quietShellCmd' cmd args

-- Should it report _what_ call failed?
checkExitCode :: ExitCode -> Aura ()
checkExitCode = checkExitCode' ""

checkExitCode' :: String -> ExitCode -> Aura ()
checkExitCode' s ec | S.didProcessSucceed ec = return ()
                    | otherwise              = failure s
