
module Container where

class Container c where
  hasElem :: (Eq a) => c a -> a -> Bool
  nrElems :: c a -> Int

instance Container Maybe where
  hasElem (Just x) e = x == e
  hasElem _ _ = False

  nrElems (Just _) = 1
  nrElems _ = 0

instance Container [] where
  hasElem l e = elem e l

  nrElems l = length l

