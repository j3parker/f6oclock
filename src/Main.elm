module Main exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (class, href)
import Html.Keyed
import Http
import Mouse
import PageVisibility exposing (..)
import Reddit exposing (Post, decodeListing)
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
    | RisingPosts (Result Http.Error (List Post))
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
                    ( { model | loop = Waiting }, getRisingPosts )

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

        RisingPosts (Ok posts) ->
            let
                sortedPosts =
                    List.sortBy .upvotes posts |> List.reverse
            in
            ( { model | loop = reset, posts = sortedPosts }, Cmd.none )

        RisingPosts (Err _) ->
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


getRisingPosts : Cmd Msg
getRisingPosts =
    let
        url =
            "https://www.reddit.com/r/politics/rising.json"

        req =
            Http.get url decodeListing
    in
    Http.send RisingPosts req


view : Model -> Html Msg
view model =
    List.concatMap renderKeyedPost model.posts |> Html.Keyed.node "div" [ class "listing" ]


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
        color =
            intensity post.upvotes |> class
    in
    [ Html.span [ class "ref", color ] [ text post.domain ]
    , Html.a [ href post.url, class "storyLink", color ] [ text post.title ]
    , Html.a [ href post.permalink, class "commentsLink", color ] [ text "comments" ]
    ]


intensity : Int -> String
intensity v =
    let
        normV =
            (toFloat v - 100) / 600 |> max 0 |> min 1
    in
    round (normV * 13) |> toString |> (++) "i"
