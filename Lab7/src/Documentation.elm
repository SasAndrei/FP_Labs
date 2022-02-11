
module Documentation exposing (..)

{-| Returns all the ends (tails) of a list

    tails [1, 2, 3] --> [[1, 2, 3], [2, 3], [3], []]
-}
tails : List a -> List (List a)
tails l =
    case l of
        [] -> [[]]
        x::xs -> (x::xs)::tails xs

-- [1, 2] -> 1::[2]::[2]


{-| Returns all combinations for given list of elements.
Combinations are **not** returned in lexicographic order.

```
    combinations [1, 2] --> [[1, 2], [1], [2], []]
    combinations [1, 2, 3] --> [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
```

-}
combinations : List a -> List (List a)
combinations l = 
    let
        combinationsN : Int -> List a -> List (List a)
        combinationsN n lx = 
            case (n, lx) of
                (0, _) -> [[]]
                (_, []) -> [[]]
                (num, x::xs) -> (List.map ((::) x) <| combinationsN (num - 1) xs) ++ (combinationsN num xs)
    in
        combinationsN (List.length l) l


