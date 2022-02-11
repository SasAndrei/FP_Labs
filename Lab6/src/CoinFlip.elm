
module CoinFlip exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type CoinSide
  = Heads
  | Tails

type alias Model =
  { currentFlip : Maybe CoinSide
  , flips: List CoinSide
  , nHeads : Int
  , nTails : Int
  }

initModel = Model Nothing [] 0 0

init : () -> (Model, Cmd Msg)
init _ =
  ( initModel
  , Cmd.none
  )

type Msg
  = Flip Int
  | AddFlip (List CoinSide)

  -- [Heads, Tails, Heads, Tails, Tails]
  -- [1, 0, 1, 0, 0]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    isHeads : CoinSide -> Int
    isHeads coin = if coin == Heads then 1 else 0

    isTails : CoinSide -> Int
    isTails coin = if coin == Tails then 1 else 0
  in
  case msg of
    Flip nFlips ->
      ( model
      , Random.generate AddFlip (Random.list nFlips coinFlip)
      )


    AddFlip coins ->
      ( Model (coins |> List.reverse |> List.head) (coins ++ model.flips) (List.map isHeads coins |> List.sum |> (+) model.nHeads)
        (List.map isTails coins |> List.sum |> (+) model.nTails)
      , Cmd.none
      )

coinFlip : Random.Generator CoinSide
coinFlip =
  Random.uniform Heads
    [ Tails ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  let
    currentFlip = 
      model.currentFlip 
      |> Maybe.map viewCoin
      |> Maybe.withDefault (text "Press the flip button to get started")
    flips = 
      model.flips 
      |> List.map coinToString
      |> List.intersperse " "
      |> List.map text
  in
    div []
      [ button [ onClick <| Flip 1] [ text "Flip" ]
      , button [ onClick <| Flip 10] [ text "Flip 10" ]
      , button [ onClick <| Flip 100] [ text "Flip 100" ]
      , currentFlip
      , div [] flips
      , div [] [
        p [] [text <| "Number of heads: " ++ String.fromInt model.nHeads]
        , p [] [text <| "Number of tails: " ++ String.fromInt model.nTails]
      ] 
      ]

coinToString : CoinSide -> String
coinToString coin =
  case coin of
    Heads -> "h"
    Tails -> "t"

viewCoin : CoinSide -> Html Msg
viewCoin coin =
  let
    name = coinToString coin
  in
    div [ style "font-size" "4em" ] [ text name ]

