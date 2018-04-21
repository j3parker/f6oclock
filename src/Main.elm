module Main exposing (..)

import Colour exposing (Lab, Rgb, labLerp, toCss)
import Html exposing (Html, text)
import Html.Attributes exposing (class, href, style, title)
import Html.Keyed
import Http
import Mouse
import PageVisibility exposing (..)
import Reddit exposing (Post, fetchPosts)
import SanitizeUrl exposing (sanitizeUrl)
import Time exposing (Time, second)


type LoopState
    = Ready Int -- countdown until Reddit refresh
    | Choked -- because mouse moved
    | Waiting -- indicates we are waiting on a response from Reddit


type alias Model =
    { visibility : Visibility
    , loop : LoopState
    , posts : List Post
    }


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | FetchResult (Result Http.Error (List Post))
    | VisibilityChange Visibility


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( Model Visible (Ready 1) [], Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ visibilityChanges VisibilityChange ] ++ modelSubscriptions model |> Sub.batch


modelSubscriptions : Model -> List (Sub Msg)
modelSubscriptions model =
    case ( model.visibility, model.loop ) of
        ( Visible, Ready _ ) ->
            [ Time.every second Tick
            , Mouse.moves MouseMove
            ]

        ( Visible, Choked ) ->
            -- don't listen to mouse events when choked
            [ Time.every second Tick ]

        _ ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.loop of
                Ready 1 ->
                    -- refresh
                    ( { model | loop = Waiting }, fetchPosts "politics" "rising" FetchResult )

                Ready t ->
                    -- count down
                    ( { model | loop = Ready (t - 1) }, Cmd.none )

                Choked ->
                    ( { model | loop = reset }, Cmd.none )

                Waiting ->
                    ( model, Cmd.none )

        MouseMove _ ->
            -- prevent links moving underneath the cursor by not refreshing
            ( { model | loop = Choked }, Cmd.none )

        FetchResult (Ok posts) ->
            let
                sortedPosts =
                    List.sortBy .upvotes posts |> List.reverse
            in
            ( { model | loop = reset, posts = sortedPosts }, Cmd.none )

        FetchResult (Err _) ->
            -- On error, set a longer countdown to "cool down"
            -- NOTE: visibility changes will clear this
            ( { model | loop = Ready 30 }, Cmd.none )

        VisibilityChange Visible ->
            -- immediately refresh when we become visible
            ( { model | visibility = Visible, loop = Ready 1 }, Cmd.none )

        VisibilityChange Hidden ->
            ( { model | visibility = Hidden }, Cmd.none )


reset : LoopState
reset =
    Ready 4


view : Model -> Html Msg
view model =
    let
        listing =
            List.concatMap renderKeyedPost model.posts |> Html.Keyed.node "div" [ class "listing" ]

        srcLink =
            Html.a [ href "https://github.com/j3parker/f6oclock", class "srcLink" ] [ text "[src]" ]
    in
    Html.div [] [ listing, srcLink ]


renderKeyedPost : Post -> List ( String, Html Msg )
renderKeyedPost post =
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


renderPost : Post -> List (Html Msg)
renderPost post =
    let
        anim =
            class "smoothBg"

        color =
            intensity post.upvotes

        style_ =
            style [ ( "backgroundColor", toCss color ) ]
    in
    [ Html.a [ class "ref", anim, style_, href post.url ] [ text post.domain ]
    , Html.a [ class "storyLink", anim, style_, href (sanitizeUrl post), title post.title ] [ text post.title ]
    , Html.a [ class "commentsLink", anim, style_, href post.permalink ] [ text "comments" ]
    ]


intensity : Int -> Rgb
intensity =
    labLerp (Lab 100 0 0) (Lab 57 63.5 46) 20 700
