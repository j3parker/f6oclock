module View exposing (view)

import Colour exposing (Lab, Rgb, labLerp, toCss)
import Html exposing (Html, text)
import Html.Attributes exposing (class, href, style, title)
import Html.Keyed
import Model exposing (Model)
import Reddit exposing (Post)
import Route
import SanitizeUrl exposing (sanitizeUrl)


view : Model -> Html msg
view model =
    let
        ( intensityAttrib, low, high ) =
            case model.route of
                Route.Index ->
                    ( "backgroundColor", Lab 100 0 0, Lab 57 63.5 46 )

                Route.Ninja ->
                    ( "color", Lab 98 0 0, Lab 0 0 0 )

        render =
            renderPost intensityAttrib (intensity low high) |> renderKeyedPost

        listing =
            List.concatMap render model.posts
                |> Html.Keyed.node "div" [ class "listing" ]

        srcLink =
            Html.a [ href "https://github.com/j3parker/f6oclock", class "srcLink" ] [ text "[src]" ]
    in
    Html.div [] [ listing, srcLink ]


renderKeyedPost : (Post -> List (Html msg)) -> Post -> List ( String, Html msg )
renderKeyedPost renderPost post =
    let
        keys =
            getPostElemKeys post

        elems =
            renderPost post
    in
    List.map2 (,) keys elems


getPostElemKeys : Post -> List String
getPostElemKeys post =
    List.map
        (\n -> post.id ++ "-" ++ toString n)
        (List.range 1 3)


renderPost : String -> (Int -> Rgb) -> Post -> List (Html msg)
renderPost intensityAttrib intensity post =
    let
        anim =
            class "smooth"

        color =
            intensity post.upvotes

        style_ =
            style [ ( intensityAttrib, toCss color ) ]
    in
    [ Html.a [ class "ref", anim, style_, href post.url ] [ text post.domain ]
    , Html.a [ class "storyLink", anim, style_, href (sanitizeUrl post), title post.title ] [ text post.title ]
    , Html.a [ class "commentsLink", anim, style_, href post.permalink ] [ text "comments" ]
    ]


intensity : Lab -> Lab -> Int -> Rgb
intensity low high =
    labLerp low high 20 700
