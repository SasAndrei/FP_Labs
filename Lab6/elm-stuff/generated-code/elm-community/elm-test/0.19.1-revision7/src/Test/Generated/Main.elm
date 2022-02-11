module Test.Generated.Main exposing (main)

import CoinFlipTests
import CounterTests
import RecipeTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 53825336600591
        , processes = 4
        , globs =
            []
        , paths =
            [ "d:\\teaching\\2021_2022\\fp\\L06\\tests\\CoinFlipTests.elm"
            , "d:\\teaching\\2021_2022\\fp\\L06\\tests\\CounterTests.elm"
            , "d:\\teaching\\2021_2022\\fp\\L06\\tests\\RecipeTests.elm"
            ]
        }
        [ ( "CoinFlipTests"
          , [ Test.Runner.Node.check CoinFlipTests.initialViewTest
            ]
          )
        , ( "CounterTests"
          , [ Test.Runner.Node.check CounterTests.viewHasTwoButtons
            , Test.Runner.Node.check CounterTests.viewContainsTheCurrentCount
            , Test.Runner.Node.check CounterTests.buttonDisabledOver10
            ]
          )
        , ( "RecipeTests"
          , [ Test.Runner.Node.check RecipeTests.atLeastOneIngredient
            , Test.Runner.Node.check RecipeTests.atLeastOneIngredientClass
            , Test.Runner.Node.check RecipeTests.eachIngredientHasClassIngredient
            ]
          )
        ]