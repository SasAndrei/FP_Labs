module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)
import List exposing (member)
import Html.Attributes exposing (value)
import Dict exposing (remove)
import Dict exposing (filter)
import Html.Attributes exposing (selected)
import Dict exposing (foldr)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories : List EventCategory
eventCategories =
    [ Academic, Work, Project, Award ] 


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories
    = SelectedEventCategories {selected : List EventCategory}


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected = SelectedEventCategories {selected = [Academic, Work, Project, Award]}
    --Debug.todo "Implement Model.Event.Category.allSelected"

listOfSelectedEvents: SelectedEventCategories -> List EventCategory
listOfSelectedEvents (SelectedEventCategories se) = se.selected


{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    let
        ls = listOfSelectedEvents current
    in
        member category ls
    --Debug.todo "Implement Model.Event.Category.isEventCategorySelected"

remove : List a -> a -> List a
remove ls elem = 
    case ls of
       [] -> []
       x::xs -> if x == elem then remove xs elem else x::(remove xs elem)

{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    let
        ls = listOfSelectedEvents current
    in
        if (value == False) then  SelectedEventCategories {selected = (remove ls category)} 
        else SelectedEventCategories {selected = (category::ls)} 
    --Debug.todo "Implement Model.Event.Category.set"


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
    div[][(checkbox "Academic" (isEventCategorySelected Academic model) Academic),
    (checkbox "Work" (isEventCategorySelected Work model) Work),
    (checkbox "Project" (isEventCategorySelected Project model) Project),
    (checkbox "Award" (isEventCategorySelected Award model) Award)]
    --Debug.todo "Implement the Model.Event.Category.view function"
