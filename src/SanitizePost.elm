module SanitizePost exposing (sanitizePost)

import Reddit exposing (Post)


sanitizePost : Post -> Post
sanitizePost post =
    if post.domain |> whitelist then
        post
    else
        { post | url = outline post.url }

whiteList : List String
whiteList =
    [ "self.politics", "reddit.com", "outline.com" ]

whitelist : String -> Bool
whitelist domain =
    List.member domain whiteList

outline : String -> String
outline url =
    "https://outline.com/" ++ url
