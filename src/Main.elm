module Main exposing (..)

import Http
import Model exposing (LoopState(..), Model)
import Mouse
import Navigation exposing (Location)
import PageVisibility exposing (Visibility(..), visibilityChanges)
import Reddit exposing (Post, fetchPosts)
import Route exposing (Route)
import Time exposing (Time, second)
import View


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | FetchResult (Result Http.Error (List Post))
    | VisibilityChange Visibility
    | RouteChange Route


main =
    Navigation.program (Route.fromLocation >> RouteChange)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = View.view
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Route.fromLocation location

        loopInit =
            Ready 1
    in
    ( Model Visible loopInit [] route, Cmd.none )


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

        RouteChange route ->
            ( { model | route = route }, Cmd.none )


reset : LoopState
reset =
    Ready 4
