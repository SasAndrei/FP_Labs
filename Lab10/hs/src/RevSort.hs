
module RevSort where

import Data.List as L

newtype Rev a = Rev a deriving (Show, Eq)

sortRev :: (Ord a) => [a] -> [a]
sortRev = L.sortOn Rev

