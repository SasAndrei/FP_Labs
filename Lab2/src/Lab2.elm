module Lab2 exposing (..)


type alias Point = {x: Float, y: Float}

type ShapeType
    = Circle {center: Point, radius: Float}
    | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
    | Triangle {pointA: Point, pointB: Point, pointC: Point}

distance2Points: Point -> Point -> Float
distance2Points a b =
    sqrt ((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y))

heron: Float -> Float -> Float -> Float
heron a b c =
    let
        s = (a + b + c) / 2
    in
        sqrt (s * (s - a) * (s - b) * (s - c))

pointInShape: ShapeType -> Point -> Bool
pointInShape shape p =
    case shape of
        Circle cir -> if distance2Points cir.center p < cir.radius then True else False
        Rectangle rec -> if rec.topLeftCorner.x < p.x then True else False
        Triangle trg -> if trg.pointA.x < p.x then True else False
