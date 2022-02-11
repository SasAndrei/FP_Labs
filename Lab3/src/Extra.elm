module Extra exposing (..)

heron : Float -> Float -> Float -> Float
heron a b c =
    let
        s = (a + b + c) / 2
    in
        sqrt (s * (s - a) * (s - b) * (s - c))

type Shape
    = Circle {radius : Float}
    | Rectangle { width : Float, height : Float}
    | Triangle { sideA : Float, sideB : Float, sideC : Float}


areaRec : Shape -> Float
areaRec shape = 
    case shape of
        Circle {radius} -> pi * radius * radius
        Rectangle { width, height} -> width * height
        Triangle { sideA, sideB, sideC} -> heron sideA sideB sideC


validTriangle : Float -> Float -> Float -> Bool
validTriangle a b c =
    ((a > 0) && (b > 0) && (c > 0)) && ((a + b >= c) && (a + c >= b) && (b + c >= a))


safeHeron : Float -> Float -> Float -> Maybe Float
safeHeron a b c =
    if not (validTriangle a b c) then
        Nothing
    else
        Just (heron a b c)



safeArea : Shape -> Result String Float
safeArea shape =
    case shape of
        Circle {radius} ->
            if radius < 0 then
                Err "Negative circle radius"
            else
                Ok (pi * radius * radius)
        Rectangle {width, height} ->
            if (width < 0) || (height < 0) then
                Err "Negative rectangle width or height"
            else
                Ok (width * height)
        Triangle {sideA, sideB, sideC} ->
            case safeHeron sideA sideB sideC of
                Just area -> Ok area
                Nothing -> Err "Sides can’t form a triangle"


type Order = LT | EQ | GT


comparare: Float -> Float -> Order
comparare a b =
    if a < b then LT else if a > b then GT else EQ


errString: Result String Float -> String
errString res = 
  case res of 
    Ok _ -> "OK"
    Err(e) -> e


--Exercise 3.5.7
cmpShapes: Shape -> Shape -> Result String Order
cmpShapes shape1 shape2 =
    case (safeArea shape1, safeArea shape2) of
        ( Ok _ , Ok _ ) -> Ok (comparare (areaRec shape1) (areaRec shape2) )
        ( Err _ , Ok _)->Err ("Invalid input for left shape:" ++ errString(safeArea shape1))
        ( Ok _ , Err _)->Err ("Invalid input for right shape:" ++ errString(safeArea shape2))
        (  _ , _)->Err ("Invalid input for both shapes" ++ errString(safeArea shape1) ++ " and " ++ errString(safeArea shape2))


type InvalidShapeError = InvalidCircle | InvalidRectangle | InvalidTriangle


--Exercise 3.5.8
totalArea : List Shape -> Result (Int, InvalidShapeError) Float
totalArea ls =
    case ls of
        [] ->  Ok 0
        x::xs -> Err (0, InvalidRectangle)