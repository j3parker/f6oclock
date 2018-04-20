module SanitizePost exposing (sanitizePost)

import Reddit exposing (Post)


sanitizePost : Post -> Post
sanitizePost post =
    if post.domain |> paywalled then
        { post | url = depaywall post.url }
    else
        post


paywalled : String -> Bool
paywalled domain =
    case domain of
        "washingtonpost.com" ->
            True

        _ ->
            False


depaywall : String -> String
depaywall url =
    "https://outline.com/" ++ url
