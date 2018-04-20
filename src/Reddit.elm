module Reddit exposing (Post, fetchPosts)

import Http
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


fetchPosts : String -> String -> (Result Http.Error (List Post) -> a) -> Cmd a
fetchPosts subreddit feed handler =
    let
        url =
            "https://www.reddit.com/r/" ++ subreddit ++ "/" ++ feed ++ ".json"

        req =
            Http.get url decodeListing
    in
    Http.send handler req


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
