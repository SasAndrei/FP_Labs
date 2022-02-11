
module CounterClass exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (class)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type alias Model = Int

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

redText c = if c then [style "color" "red"] else []

view : Model -> Html Msg
view model =
  let
    bigFont = style "font-size" "20pt"
  in
    div []
      [ button [ bigFont, onClick Increment, disabled (model >= 10)] [ text "+" ]
      , div ([ bigFont ] ++ redText (model <= -8 || model >= 8)) [ text (String.fromInt model) ]
      , button [ bigFont, onClick Decrement, disabled (model <= -10)] [ text "-" ]
      ]

