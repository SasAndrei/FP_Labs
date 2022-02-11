module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import List exposing (sortWith)
import Html.Attributes exposing (href)


type alias Event =  
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


sortByInterval : List Event -> List Event
sortByInterval events = sortWith (\x y -> Interval.compare (x.interval) (y.interval)) events

maybeToString: Maybe String -> String
maybeToString s =
    case s of
       Just x -> x
       Nothing -> ""

view : Event -> Html Never
view event =
    div [classList [("event", True), ("event-important", event.important)]] 
    [ h1 [class "event-title"] [text event.title],
    p[class "event-category"][categoryView event.category],
    p[class "event-url"][a [href (maybeToString event.url)][]],
    p[class "event-interval"][Interval.view event.interval],
    p[class "event-description"][event.description]]
