module Extra exposing (..)


type alias Point = {x: Int, y: Int, z: Int}


type Color = Red | Green | Blue


type alias ColoredSphere = {center: Point, color: Color, radius: Int}


moveUpdatePoint : Point -> Int -> Int -> Point
moveUpdatePoint point dx dy = {point | x = point.x + dx, y = point.y + dy}


moveUpdate : ColoredSphere -> Int -> Int -> ColoredSphere
moveUpdate cs mx my =  {cs | center = (moveUpdatePoint cs.center mx my)}


splitLast : List a -> Maybe (List a, a)
splitLast ls =
    case ls of
        [] -> Nothing
        x::[] -> Just ([], x)
        x::xs -> Just (xs, x)