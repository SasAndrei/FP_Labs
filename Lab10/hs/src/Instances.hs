
module Instances where

import Data.List

data Color = Red | Green | Blue

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

data Medal = Gold | Silver | Bronze deriving (Show, Eq)

instance Ord Medal where
  compare Gold Gold = EQ
  compare Gold _ = GT
  compare Silver Silver = EQ
  compare Silver Bronze = GT
  compare Silver Gold = LT
  compare Bronze Bronze = EQ
  compare Bronze _ = LT

instance Enum Medal where
  toEnum 0 = Bronze
  toEnum 1 = Silver
  toEnum 2 = Gold

  fromEnum Bronze = 0
  fromEnum Silver = 1
  fromEnum Gold = 2

instance Bounded Medal where
  minBound = Bronze
  maxBound = Gold

medals :: [Medal]
medals = [minBound..maxBound]

