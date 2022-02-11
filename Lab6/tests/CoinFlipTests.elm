
module CoinFlipTests exposing (..)

import CoinFlip
import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz
import Test.Html.Query as Q
import Test.Html.Selector as S
import Html.Attributes as Attr

initialViewTest : Test
initialViewTest = 
    test "When the app is started, there should be text saying: Press the.." <|
        \_ -> 
            CoinFlip.view (CoinFlip.initModel)
                |> Q.fromHtml
                |> Q.has [ S.text "Press the flip button to get started" ]

