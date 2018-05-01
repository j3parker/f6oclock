module Model exposing (LoopState(..), Model)

import PageVisibility exposing (Visibility)
import Reddit exposing (Post)
import Route exposing (Route)


type LoopState
    = Ready Int -- countdown until Reddit refresh
    | Choked -- because mouse moved
    | Waiting -- indicates we are waiting on a response from Reddit


type alias Model =
    { visibility : Visibility
    , loop : LoopState
    , posts : List Post
    , route : Route
    }
