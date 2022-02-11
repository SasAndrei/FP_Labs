module Exercise where
import GHC.TypeLits (Div)

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Tree a = Nil | Node (Tree a) a (Tree a)

class Container c where
    hasElem :: (Eq a) => c a -> a -> Bool
    nrElems :: c a -> Int

instance Container Tree where
    hasElem Nil _ = False
    hasElem (Node x y z) e = ((y == e) || (hasElem x e) || (hasElem z e))

    nrElems _ = 1