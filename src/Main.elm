module Main exposing (..)

import Html exposing (Html, span, text)
import Http
import Json.Decode exposing (..)
import Mouse
import PageVisibility exposing (..)
import Time exposing (Time, second)


type LoopState
    = Ready Int -- countdown until Reddit refresh
    | Waiting -- indicates we are waiting on a response from Reddit


type alias Model =
    { loop : LoopState
    , data : String
    , visibility : Visibility
    }


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | RisingPosts (Result Http.Error String)
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
    ( Model (Ready 1) "" Visible, Cmd.none )


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

                Ready _ ->
                    -- count down
                    ( { model | loop = tick model.loop }, Cmd.none )

                Waiting ->
                    ( model, Cmd.none )

        MouseMove _ ->
            -- prevent links moving underneath the cursor by not refreshing
            ( { model | loop = reset }, Cmd.none )

        RisingPosts (Ok data) ->
            ( { model | loop = reset, data = data }, Cmd.none )

        RisingPosts (Err _) ->
            -- On error, set a longer countdown to "cool down"
            -- NOTE: visibility changes will clear this
            ( { model | loop = Ready 30 }, Cmd.none )

        VisibilityChange Visible ->
            -- immediately refresh when we become visible
            ( { model | visibility = Visible, loop = Ready 1 }, Cmd.none )

        VisibilityChange Hidden ->
            ( { model | visibility = Hidden }, Cmd.none )


tick : LoopState -> LoopState
tick state =
    case state of
        Ready t ->
            Ready (t - 1)

        Waiting ->
            Waiting


reset : LoopState
reset =
    Ready 3


getRisingPosts : Cmd Msg
getRisingPosts =
    let
        url =
            "https://www.reddit.com/r/politics/rising.json"

        req =
            Http.get url decodeRisingPosts
    in
    Http.send RisingPosts req


decodeRisingPosts : Decoder String
decodeRisingPosts =
    field "kind" string


view : Model -> Html Msg
view model =
    span [] [ text (model.data ++ " " ++ toString model.loop) ]
