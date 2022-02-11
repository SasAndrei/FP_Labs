module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import Json.Decode exposing (nullable)
import List exposing (sortBy)


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


maybeToString: Maybe String -> String
maybeToString s =
    case s of
       Just x -> x
       Nothing -> ""


view : Repo -> Html msg
view repo =
    div [class "repo"] [
    p[class "repo-name"][text repo.name],
    p[class "repo-description"][text (maybeToString repo.description)],
    p[class "repo-url"][a [href repo.url][text repo.pushedAt]],
    p[class "repo-stars"][text (String.fromInt(repo.stars))]
    ]
    --Debug.todo "Implement Model.Repo.view"



sortByStars : List Repo -> List Repo
sortByStars repos =
    sortBy .stars repos
    --Debug.todo "Implement Model.Repo.sortByStars"


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.maybe (De.field "description" De.string))
        (De.field "url" De.string)
        (De.field "pushedAt" De.string)
        (De.field "stars" De.int)
    --Debug.todo "Implement Model.Repo.decodeRepo"
