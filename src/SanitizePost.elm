module SanitizePost exposing (sanitizePost)

import Reddit exposing (Post)


sanitizePost : Post -> Post
sanitizePost post =
    if post.domain |> whitelist then
        post
    else
        { post | url = depaywall post.url }


whitelist : String -> Bool
whitelist domain =
    case domain of
        "self" ->
	    -- Guessing in the dark about selfposts
            True

        _ ->
            False


depaywall : String -> String
depaywall url =
    "https://outline.com/" ++ url
