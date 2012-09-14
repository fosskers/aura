-- THIS NEEDS TO BE TAKEN OVER BY MonadPlus!!

module Zero where

import System.Exit(ExitCode(..))

-- A type that is part of the Zero class must define a function
-- `zero` which returns its `most null` or `most plain` value.
class Zero a where
    zero :: a
    isZero :: a -> Bool

instance Zero Int where
    zero = 0
    isZero = zeroDefault

instance Zero a => Zero (Maybe a) where
    zero = Nothing
    isZero Nothing  = True
    isZero (Just _) = False

instance Zero a => Zero [a] where
    zero = []
    isZero xs = null xs

instance Zero Char where
    zero = '\NUL'  -- Is there a better value for this?
    isZero = zeroDefault    

instance Zero Bool where
    zero = False
    isZero = zeroDefault

instance Zero ExitCode where
    zero = ExitFailure 1
    isZero = zeroDefault

-- Not done yet.
instance (Zero a, Zero b) => Zero (Either a b) where
    zero = Left (zero :: a)
    isZero (Left _)  = True
    isZero (Right _) = False

zeroDefault :: (Eq a, Zero a) => a -> Bool
zeroDefault x = x == zero