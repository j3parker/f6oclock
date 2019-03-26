module SanitizeUrl exposing (sanitizeUrl)

import Reddit exposing (Post)


sanitizeUrl : Post -> String
sanitizeUrl post =
    if post.domain |> whitelist then
        post.url
    else
        outline post.url

whiteList : List String
whiteList =
    [ "self.politics", "reddit.com", "outline.com", "nytimes.com", "www.nytimes.com" ]

whitelist : String -> Bool
whitelist domain =
    List.member domain whiteList

outline : String -> String
outline url =
    "https://outline.com/" ++ url
