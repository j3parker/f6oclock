module Loop exposing (Msg(..), State(..), init, update)


type State
    = Ready Int -- countdown until Reddit refresh
    | Choked -- because mouse moved
    | Waiting -- indicates we are waiting on a response from Reddit


type Msg
    = Tick
    | Reset
    | Choke
    | Fault


init =
    Ready 1


reset =
    Ready 4


update : Msg -> State -> Cmd msg -> ( State, Cmd msg )
update msg state trigger =
    case ( msg, state ) of
        ( Reset, _ ) ->
            ( reset, Cmd.none )

        ( Fault, _ ) ->
            -- On error, set a longer countdown to "cool down"
            -- NOTE: visibility changes will clear this
            ( Ready 30, Cmd.none )

        ( _, Waiting ) ->
            ( state, Cmd.none )

        ( Tick, Ready 1 ) ->
            ( Waiting, trigger )

        ( Tick, Ready t ) ->
            ( Ready (t - 1), Cmd.none )

        ( Tick, Choked ) ->
            ( reset, Cmd.none )

        ( Choke, _ ) ->
            ( Choked, Cmd.none )
