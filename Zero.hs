module Zero where

-- A type that is part of the Zero class must define a function
-- `zero` which returns its `most null` or `most plain` value.
class Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero a => Zero (Maybe a) where
    zero = Nothing

instance Zero a => Zero [a] where
    zero = []

instance Zero Char where
    zero = '\NUL'  -- Is there a better value for this?

instance Zero Bool where
    zero = False