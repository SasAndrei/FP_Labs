module PracticeProblems exposing (..)

-- Ex. 4.6.1
-- enumerate ['a', 'b', 'c'] => [(0, 'a'), (1, 'b'), (2, 'c')]
enumerate : List a -> List (Int, a)
enumerate l = 
    let
        enumerateWithIndex lx i =
            case lx of
                [] -> []
                x::xs -> (i, x)::(enumerateWithIndex xs (i + 1))
    in
        enumerateWithIndex l 0
        

-- Ex. 4.6.2
-- repeat n elem (repeat 4 'a' => ['a', 'a', 'a', 'a'])
repeat : Int -> a -> List a 
repeat n elem = 
    if n == 0 then [] else elem::(repeat (n - 1) elem)
    

-- Ex. 4.6.3
-- countVowels word 
isVowel : Char -> Bool
isVowel v = List.member (Char.toUpper v) ['A', 'E', 'I', 'O', 'U']

-- String.toList "Hello" => ['H', 'e', 'l', 'l', 'o']
countVowels : String -> Int
countVowels word = 
    List.length (List.filter isVowel (String.toList word))
    

-- Ex. 4.6.4
-- partition : comparable -> List comparable -> (List comparable, List comparable)
-- partition pivot l =
--      (filter (\x -> x < pivot) l, filter (\x -> x >= pivot) l)
partition : comparable -> List comparable -> (List comparable, List comparable)
partition pivot l = 
    let
        partitionSmaller piv lx = 
            case lx of
               [] -> []
               x::xs -> if x < piv then x::partitionSmaller piv xs else partitionSmaller piv xs
               
        partitionLarger piv lx = 
            case lx of
               [] -> []
               x::xs -> if x >= piv then x::partitionLarger piv xs else partitionLarger piv xs
    in
        (partitionSmaller pivot l, partitionLarger pivot l)
        

-- Ex. 4.6.5
countriesWithCapital : List (String, String) -> (String -> Bool) -> List String
countriesWithCapital countries predicate = List.map(\(a, _) -> a) (List.filter (\(_, b) -> predicate b) countries)


-- Ex. 4.6.6
filterMap : (a -> Maybe b) -> List a -> List b 
filterMap fm l = 
    case l of 
        [] -> []
        x::xs -> case fm x of
                    Nothing -> filterMap fm xs
                    Just r -> r::filterMap fm xs

    
-- Ex. 4.6.7.
-- all, any using foldl
-- p = de ex, (\x -> x < 3)
all p = List.foldl (\x y -> p x && y) True
any p = List.foldl (\x y -> p x || y) False


-- Ex. 4.6.8
-- chunks n l
-- chunks 2 [1,2,3,4,5,6] = [[1,2], [3,4], [5, 6]]
-- chunks 3 [1,2,3,4,5,6] = [[1,2,3], [4,5,6]]
-- chunks 5 [1,2,3,4,5,6] = [[1,2,3,4,5], [6]]
chunks: Int -> List a -> List (List a)
chunks n l =
    case l of
        [] -> []
        _ -> (List.take n l)::(chunks n (List.drop n l))