
module Countries exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, checked, type_, value, placeholder)
import Html.Events exposing (..)
import Http
import Json.Decode as Dec


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Country =
    { name : String
    , area : Float
    , region : String
    , population : Float
    }


decodeCountry : Dec.Decoder Country
decodeCountry =
    Dec.map4 Country 
        (Dec.at  ["name", "common"] Dec.string)
        (Dec.field "area" Dec.float)
        (Dec.field "region" Dec.string)
        (Dec.field "population" Dec.float)

type Model
    = Initial
    | RequestSent
    | Success (List Country) String Bool String
    | Error Http.Error

init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial
    , Cmd.none
    )


type Msg
    = GetCountries
    | GotCountries (Result Http.Error (List Country))
    | ChangeSortingOrder Bool
    | ChangeSortingCriterion String
    | ChangeNameFilter String

getCountries : Cmd Msg
getCountries = Http.get 
    { url = "https://restcountries.com/v3.1/all"
    , expect = Http.expectJson GotCountries (Dec.list decodeCountry) 
    }

compareByDensity : Country -> Country -> Order
compareByDensity c1 c2 =
    let
        d1 = c1.population / c1.area
        d2 = c2.population / c2.area
    in
        if d1 > d2 then GT else if d1 < d2 then LT else EQ

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        ChangeNameFilter nameFilter ->
            case model of 
                Success countries criterion ascending _ -> (Success countries criterion ascending nameFilter, Cmd.none)
                _ -> (Initial, Cmd.none)

        ChangeSortingCriterion criterion ->
            case model of 
                Success countries _ ascending nameFilter -> (Success countries criterion ascending nameFilter, Cmd.none)
                _ -> (Initial, Cmd.none)

        ChangeSortingOrder ascending ->
            case model of
                Success countries criterion _ nameFilter-> (Success countries criterion ascending nameFilter, Cmd.none)
                _ -> (Initial, Cmd.none)

        GetCountries ->
            ( RequestSent
            , getCountries
            )

        GotCountries (Ok countries) ->
            ( Success countries "pop" True ""
            , Cmd.none
            )

        GotCountries (Err err) ->
            ( Error err
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



view : Model -> Html Msg
view model =
    case model of
        Initial ->
            viewInitial

        RequestSent ->
            div [] [ text "Loading..." ]

        Success countries criterion ascending nameFilter ->
            viewSuccess countries criterion ascending nameFilter

        Error err ->
            viewError err

viewInitial : Html Msg
viewInitial =
    div []
        [ button [ onClick GetCountries ] [ text "Get countries" ]
        ]

viewCountry : Country -> Html msg
viewCountry {name, area, region, population} =
    div [style "border" "solid 1px", style "margin" "2px"] 
        [ p [] [text <| "Name:" ++ name]
        , p [] [text <| "Area: " ++ String.fromFloat area]
        , p [] [text <| "Population: " ++ String.fromFloat population]
        , p [] [text <| "Density: " ++ String.fromFloat (population / area)]
        ]



viewSuccess : List Country -> String -> Bool -> String -> Html Msg
viewSuccess countries criterion ascending nameFilter =
    let
        countryNameFilter : String -> Country -> Bool
        countryNameFilter filterString country = 
            if String.contains filterString country.name then True else False

        sortedCountries = case criterion of
            "pop" -> List.sortBy .population countries
            "area" -> List.sortBy .area countries
            "density" -> List.sortWith compareByDensity countries
            _ -> []
        filteredCountries = 
            if nameFilter == "" then 
                sortedCountries
            else 
                List.filter (countryNameFilter nameFilter) sortedCountries
    in
    div [] <|
    [
        h2 [] [text "ok"]
        , input [ type_ "checkbox", onCheck ChangeSortingOrder, checked ascending] []
        , text "Sort in ascending order by: "
        , div [] [ select [ Html.Events.onInput ChangeSortingCriterion ]
                    [ option [ value "pop" ] [ text "Population" ]
                    , option [ value "area" ] [ text "Area" ]
                    , option [ value "density" ] [ text "Density" ]
                    ]
                    ]
        , div [] [ input [ type_ "input", placeholder "Type country name..", value nameFilter, onInput ChangeNameFilter ] []]
    ] ++ (List.map (viewCountry) <| if ascending then filteredCountries else (List.reverse filteredCountries))

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "Bad Url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "Bad Status: " ++ String.fromInt status

        Http.BadBody _ ->
            "Bad Body"


viewError : Http.Error -> Html msg
viewError err =
    div [] [ h2 [] [ text "Rip" ], p [] [ text <| httpErrorToString err ] ]


    

