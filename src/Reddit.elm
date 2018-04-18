module Reddit exposing (Post, decodeListing)

import Json.Decode exposing (..)


type alias Post =
    { id : String
    , title : String
    , domain : String
    , permalink : String
    , url : String
    , upvotes : Int
    , createdUtc : Int
    }


decodeListing : Decoder (List Post)
decodeListing =
    at [ "data", "children" ] (list decodePost)


decodePost : Decoder Post
decodePost =
    field "data" decodePostData


decodePostData : Decoder Post
decodePostData =
    map7 Post
        (field "id" string)
        (field "title" string)
        (field "domain" string)
        (map (\rel -> "https://www.reddit.com" ++ rel) (field "permalink" string))
        (field "url" string)
        (field "ups" int)
        (field "created_utc" int)
