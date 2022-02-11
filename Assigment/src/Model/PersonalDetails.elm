module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Attributes exposing (href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

foldr : (a -> b -> b) -> b -> List a -> b
foldr op start l =
    case l of
        [] -> start
        x::xs -> op x (foldr op start xs)

connect : DetailWithName -> String -> String
connect detail ls = detail.detail ++ ls

nameconn : DetailWithName -> String -> String
nameconn detail ls = detail.detail ++ ls
    

view : PersonalDetails -> Html msg
view details = 
    div [] 
    [ h1 [id "name"] [text details.name],
      em [id "intro"] [text details.intro],
      p [class "contact-detail"] [text (foldr connect "" (details.contacts))],
      p [class "social-link"][a [href (foldr connect "" (details.socials))] [text (foldr nameconn "" (details.socials))]]]
