
module YesNo where

class YesNo a where
  yesno :: a -> Bool

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo Bool where
  yesno b = b

class YesNoMay c where
  yesnoMay :: (Eq a) => c a -> a -> Bool

instance YesNoMay Maybe where
  yesnoMay (Just x) e = x == e
  yesnoMay _ _ = False

class YesNoLen a where
  yesnoLen :: a -> Int

instance (YesNo a) => YesNoLen [a] where
  yesnoLen l = length $ filter yesno l

