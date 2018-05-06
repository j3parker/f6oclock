module Main exposing (..)

import Http
import Loop
import Mouse
import Navigation exposing (Location)
import PageVisibility exposing (Visibility(..), visibilityChanges)
import Reddit exposing (Post, fetchPosts)
import Render
import Route exposing (Route)
import Time exposing (Time, second)
import Tuple


type alias Model =
    { visibility : Visibility
    , loop : Loop.State
    , posts : List Post
    , route : Route
    }


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | FetchResult (Result Http.Error (List Post))
    | VisibilityChange Visibility
    | RouteChange Route


type alias Flags =
    { posts : List Post
    }


main =
    Navigation.programWithFlags (Route.fromLocation >> RouteChange)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = \model -> Render.render model.route model.posts
        }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            Route.fromLocation location
    in
    ( Model Visible Loop.init flags.posts route, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ visibilityChanges VisibilityChange ] ++ modelSubscriptions model |> Sub.batch


modelSubscriptions : Model -> List (Sub Msg)
modelSubscriptions model =
    case ( model.visibility, model.loop ) of
        ( Visible, Loop.Ready _ ) ->
            [ Time.every second Tick
            , Mouse.moves MouseMove
            ]

        ( Visible, Loop.Choked ) ->
            -- don't listen to mouse events when choked
            [ Time.every second Tick ]

        ( Hidden, _ ) ->
            [ Time.every (10 * second) Tick ]

        _ ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        trigger =
            fetchPosts "politics" "rising" FetchResult

        updateLoop msg =
            Loop.update msg model.loop trigger

        updateModel =
            Tuple.mapFirst (\ls -> { model | loop = ls })
    in
    case msg of
        Tick _ ->
            updateLoop Loop.Tick |> updateModel

        MouseMove _ ->
            -- prevent links moving underneath the cursor by not refreshing
            updateLoop Loop.Choke |> updateModel

        FetchResult (Ok posts) ->
            let
                sortedPosts =
                    List.sortBy .upvotes posts |> List.reverse

                newLoop =
                    updateLoop Loop.Reset

                updateModel ls =
                    { model | loop = ls, posts = sortedPosts }
            in
            Tuple.mapFirst updateModel newLoop

        FetchResult (Err _) ->
            updateLoop Loop.Fault |> updateModel

        VisibilityChange Visible ->
            -- immediately refresh when we become visible
            ( { model | visibility = Visible, loop = Loop.init }, Cmd.none )

        VisibilityChange Hidden ->
            ( { model | visibility = Hidden }, Cmd.none )

        RouteChange route ->
            ( { model | route = route }, Cmd.none )
