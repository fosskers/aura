module Aura.Settings.SettingsTest ( sampleSettings ) where

import Aura.Settings.Base
import Aura.Colour.Text
import Aura.Languages

---

sampleSettings :: Settings
sampleSettings = Settings { environmentOf = undefined
                          , langOf        = English
                          , pacmanCmdOf   = "pacman"
                          , editorOf      = "emacs"
                          , carchOf       = "x86_64"
                          , inputOf       = []
                          , pacOptsOf     = []
                          , otherOptsOf   = []
                          , ignoredPkgsOf = []
                          , wontBuildOf   = []
                          , cachePathOf   = "/var/cache/pacman/pkg"
                          , logFilePathOf = "/var/log/pacman.log"
                          , suppressMakepkg = True
                          , delMakeDeps   = False
                          , mustConfirm   = False
                          , mayHotEdit    = False
                          , diffPkgbuilds = False
                          , rebuildDevel  = False
                          , pcRed         = red
                          , pcGreen       = green
                          , pcYellow      = yellow
                          , pcBlue        = blue
                          , pcMagenta     = magenta
                          , pcCyan        = cyan
                          , pcWhite       = white }
