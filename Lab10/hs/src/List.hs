
module List where

import Data.Monoid

data List a = Nil | Cons a (List a) deriving Show

instance Semigroup (List a) where 
  l1 <> Nil = l1
  Nil <> l2 = l2
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

appLists :: (Monoid a) => [a] -> a
appLists = foldl (<>) mempty

