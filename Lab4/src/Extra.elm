module Extra exposing (..)



foldr : (a -> b -> b) -> b -> List a -> b
foldr op start l =
    case l of
        [] -> start
        x::xs -> op x (foldr op start xs)


foldl : (a -> b -> b) -> b -> List a -> b
foldl op start l =
    case l of
        [] -> start
        x::xs -> foldl op (op x start) xs


tuplet : Int -> a -> (Int , a)
tuplet i a = (i , a)


compose : a -> List (Int , a) -> List (Int , a)
compose n m =  (tuplet (List.length m) n) :: m


enum : List a -> List (Int, a)
enum l = foldl (::) [] (foldl compose [] l)


any p = List.foldl (\x y -> p x || y) False


resultErr result =
    case result of
        Ok _ -> False
        Err _ -> True


resultOk : Result err ok -> Bool
resultOk result =
    case result of
        Ok _ -> True
        Err _ -> False


filter : (a -> Bool) -> List a -> List a
filter pred l =
    case l of
        [] -> []
        x::xs -> if (pred x) then
                    x::filter pred xs
                else
                    filter pred xs


collect : List (Result err ok) -> Result err (List ok) 
collect l =
  case l of
    [] -> Ok []
    x::xs -> case (x, collect xs) of
                (Err a, _) -> Err a
                (_, Err a) -> Err a
                (Ok a, Ok lx) -> Ok (a::lx)