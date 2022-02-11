module Lab3 exposing (..)

-- Ex. 3.5.1
-- safeDiv a b
-- b == 0 => Nothing
-- else, a // b
safeDiv: Int -> Int -> Maybe Int
safeDiv a b = if b == 0 then Nothing else Just (a // b)

-- Ex. 3.5.2
-- Tail recursive list length
len : List a -> Int
len l = 
    let
        lenTR lx acc = 
            case lx of
                [] -> acc
                _::xs -> lenTR xs (acc + 1)
    in
        lenTR l 0

-- Ex. 3.5.3
-- last l 
-- Returns the last element in a list
last : List a -> Maybe a
last l = 
    case l of 
    [] -> Nothing
    x::[] -> Just x -- x::[] <=> [x]
    _::xs -> last xs


-- Ex. 3.5.4
-- indexList i l
-- Returns i-th element in list
indexList : Int -> List a -> Maybe a
indexList i l = 
    case l of
        [] -> Nothing
        x::xs -> if i == 0 then Just x else indexList (i - 1) xs


-- Ex. 3.5.5
-- fibs start end
-- Fibonacci numbers indexed [start, end)
-- fibs 0 3 => [1, 1, 2]
-- fibs 3 5 => [3 5]
fib : number -> number
fib n = if (n == 0) || (n == 1) then 1 else (fib (n - 1) + fib (n - 2))

fibs : number -> number -> List (number, number)
fibs start end = if start == end then [] else (start, (fib start))::(fibs (start + 1) end)