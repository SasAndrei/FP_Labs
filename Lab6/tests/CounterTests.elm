
module CounterTests exposing (..)

import CounterClass
import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz
import Test.Html.Query as Q
import Test.Html.Selector as S

viewHasTwoButtons : Test
viewHasTwoButtons =
    test "view contains two buttons" <|
        \_ ->
            CounterClass.view 0
                |> Q.fromHtml
                |> Q.findAll [ S.tag "button" ]
                |> Q.count (Expect.equal 2)

viewContainsTheCurrentCount : Test
viewContainsTheCurrentCount =
    test "view contains the current count" <|
        \_ ->
            CounterClass.view 0
                |> Q.fromHtml
                |> Q.has [ S.text (String.fromInt 0) ]


buttonDisabledOver10 : Test
buttonDisabledOver10 =
   test "when the counter has value 10 the + button is disabled" <|
        \_ ->
            CounterClass.view 10
                |> Q.fromHtml
                |> Q.find [ S.tag "button", S.disabled True ]
                |> Q.has [ S.text "+" ]

