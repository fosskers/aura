module Aura.Languages.Base where

import BasePrelude

data Language = English
              | Japanese
              | Polish
              | Croatian
              | Swedish
              | German
              | Spanish
              | Portuguese
              | French
              | Russian
              | Italian
              | Serbian
              | Norwegian
              | Indonesia
              | Chinese
                deriving (Eq, Enum, Ord, Read, Show)
