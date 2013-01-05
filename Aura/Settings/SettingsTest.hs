module Aura.Settings.SettingsTest ( sampleSettings ) where

import Aura.Colour.TextColouring
import Aura.Languages
import Aura.Settings.Definition

sampleSettings :: Settings
sampleSettings = Settings { environmentOf = undefined
                          , langOf        = English
                          , pacmanCmdOf   = "pacman"
                          , editorOf      = "emacs"
                          , ignoredPkgsOf = []
                          , cachePathOf   = "/var/cache/pacman/pkg"
                          , logFilePathOf = "/var/log/pacman.log"
                          , suppressMakepkg = True
                          , mustConfirm   = False
                          , mayHotEdit    = False
                          , diffPkgbuilds = False
                          , pcRed         = red
                          , pcGreen       = green
                          , pcYellow      = yellow
                          , pcBlue        = blue
                          , pcMagenta     = magenta
                          , pcCyan        = cyan
                          , pcWhite       = white }
