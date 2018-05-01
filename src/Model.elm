module Model exposing (Model)

import Loop
import PageVisibility exposing (Visibility)
import Reddit exposing (Post)
import Route exposing (Route)


type alias Model =
    { visibility : Visibility
    , loop : Loop.State
    , posts : List Post
    , route : Route
    }
