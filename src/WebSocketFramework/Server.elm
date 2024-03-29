module WebSocketFramework.Server exposing
    ( Model, Socket, Msg
    , UserFunctions, ServerMessageSender, ServerGamesDeleter, ServerPlayersDeleter
    , program
    , sendToOne, sendToMany, sendToOthers, sendToAll
    , getState, setState, getServerModel, setServerModel
    , getDeathRowDuration, setDeathRowDuration, getTime
    , otherSockets
    , verbose
    )

{-| Support for a Node.js server for WebSocketFramework messages.


# Types

@docs Model, Socket, Msg


# Callbacks

@docs UserFunctions, ServerMessageSender, ServerGamesDeleter, ServerPlayersDeleter


# Top-level program

@docs program


# Message sending

@docs sendToOne, sendToMany, sendToOthers, sendToAll


# State accessors

@docs getState, setState, getServerModel, setServerModel
@docs getDeathRowDuration, setDeathRowDuration, getTime


# Utilities

@docs otherSockets


# Model accessors

@docs verbose

-}

import Char
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra as LE
import Platform exposing (Program)
import Random
import Task
import Time exposing (Posix)
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.ServerInterface as ServerInterface
    exposing
        ( errorRsp
        , getPlayer
        , send
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , GameId
        , InputPort
        , MessageDecoder
        , MessageEncoder
        , MessageToGameid
        , OutputPort
        , PlayerId
        , PublicGames
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyServerState
        )
import WebSocketServer as WSS


{-| Create the top-level application program.

You will usually use the result of this function as the value of `main` in your top-level module.

Most servers will not need to use the `servermodel`, but it's a place to stash extra server-wide state that doesn't make sense in the game-specific `gamestate`, which is stored in the `ServerModel`, accessible via `getServerModel`.

-}
program : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Program (Maybe String) (Model servermodel message gamestate player) Msg
program servermodel userFunctions gamestate =
    Platform.worker
        { init = init servermodel userFunctions gamestate
        , update = update
        , subscriptions = subscriptions userFunctions.inputPort
        }



-- MODEL


{-| User function that is called to send the response(s) to a request.

This will usually call `sendToOne` and/or `sendToMany` with the `message` emitted by the `ServiceMessageProcessor` in the `UserFunctions` passed to `program`.

The first `message` is the request that came from client to server. The second `message` is the response. If no response is returned by the `ServiceMessageProcessor`, this function is not called.

The `ServerState` arg is the value of the `state` property of the `Model` arg, pulled out for your convenience. If you change it, you must put it back in the model you return.

-}
type alias ServerMessageSender servermodel message gamestate player =
    Model servermodel message gamestate player -> Socket -> ServerState gamestate player -> message -> message -> ( Model servermodel message gamestate player, Cmd Msg )


{-| Called when games are auto-deleted due to socket connections being lost.

This will only happen if your server code tracks the association between sockets, games and players in the `xxxDict` properties of the Model. When tracked games are dropped, this function, stored in the `gamesDeleter` property of `UserFunctions`, is called, so that you can clean up any reference to those games in your `gamestate`.

It is called BEFORE the game information is removed from the `ServerState`.

-}
type alias ServerGamesDeleter servermodel message gamestate player =
    Model servermodel message gamestate player -> List GameId -> ServerState gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )


{-| Called when players are auto-deleted due to socket connections being lost.

This will only happen if your server code tracks the association between sockets, games and players in the `xxxDict` properties of the Model. When tracked players are dropped, this function, stored in the `playersDelete` property of `UserFunctions`, is called, so that you can clean up any reference to those players in your `gamestate`.

It is called BEFORE the player information is removed from the `ServerState`.

-}
type alias ServerPlayersDeleter servermodel message gamestate player =
    Model servermodel message gamestate player -> GameId -> List PlayerId -> ServerState gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )


{-| An alias of `WebSocketServer.Socket`.
-}
type alias Socket =
    WSS.Socket


{-| User functions that get called by the generic server code.

`encodeDecode` is used to translate messages to and from strings.

`messageProcessor` processes a client request into state changes and a response message.

`messageSender` decides what to do with the response message.

`messageToGameid` extracts a GameId from a message, if there is one.

`messageToPlayerid` extracts a PlayerId from a message, if there is one.

`autoDeleteGame` is called when all sockets referencing a `GameId` have disconnected. If it returns True, then the game will be put on deathwatch, meaning it will be removed from the tables after a delay. Usually used to keep public games from being auto-deleted.

`gamesDeleter` is called when games are deleted due to their sockets being disconnected. See the `ServerGamesDeleter` description for more details.

`playersDeleter` is called when players are deleted due to their sockets being disconnected. See the `ServerPlayersDeleter` description for more details.

`inputPort` and `outputPort` are the ports used to communicate with the Node.js code.

-}
type alias UserFunctions servermodel message gamestate player =
    { encodeDecode : EncodeDecode message
    , messageProcessor : ServerMessageProcessor gamestate player message
    , messageSender : ServerMessageSender servermodel message gamestate player
    , messageToGameid : Maybe (message -> Maybe GameId)
    , messageToPlayerid : Maybe (message -> Maybe PlayerId)
    , autoDeleteGame : Maybe (GameId -> ServerState gamestate player -> Bool)
    , gamesDeleter : Maybe (ServerGamesDeleter servermodel message gamestate player)
    , playersDeleter : Maybe (ServerPlayersDeleter servermodel message gamestate player)
    , inputPort : InputPort Msg
    , outputPort : OutputPort Msg
    }


{-| An opaque type containing the application state.
-}
type Model servermodel message gamestate player
    = Model
        { servermodel : servermodel
        , userFunctions : UserFunctions servermodel message gamestate player
        , state : ServerState gamestate player
        , verbose : Bool
        , gameSocketsDict : Dict GameId (List Socket)
        , socketGamesDict : Dict Socket (List GameId)
        , playerSocketDict : Dict PlayerId Socket
        , socketPlayersDict : Dict Socket (List ( GameId, PlayerId ))
        , deathRowDuration : Int
        , deathWatch : List ( Int, GameId )
        , deathWatchGameids : Dict GameId Bool
        , deathWatchPlayers : List ( Int, GameId, PlayerId )
        , deathWatchPlayerids : Dict PlayerId Bool
        , time : Int
        }


{-| Get the servermodel from a model.
-}
getServerModel : Model servermodel message gamestate player -> servermodel
getServerModel (Model model) =
    model.servermodel


{-| Set the servermodel in a model.
-}
setServerModel : Model servermodel message gamestate player -> servermodel -> Model servermodel message gamestate player
setServerModel (Model model) servermodel =
    Model { model | servermodel = servermodel }


{-| Get the servermodel from a model.
-}
getState : Model servermodel message gamestate player -> ServerState gamestate player
getState (Model model) =
    model.state


{-| Set the ServerState in a model.
-}
setState : Model servermodel message gamestate player -> ServerState gamestate player -> Model servermodel message gamestate player
setState (Model model) state =
    Model { model | state = state }


{-| Get the death row duration from a model.

This is the time a game or player sticks around after no connections reference it.

-}
getDeathRowDuration : Model servermodel message gamestate player -> Int
getDeathRowDuration (Model model) =
    model.deathRowDuration


{-| Set the death row duration in a model.

This is the time a game or player sticks around after no connections reference it.

-}
setDeathRowDuration : Model servermodel message gamestate player -> Int -> Model servermodel message gamestate player
setDeathRowDuration (Model model) duration =
    Model { model | deathRowDuration = duration }


{-| Get the current time from a model.

The time is updated once a second.

-}
getTime : Model servermodel message gamestate player -> Int
getTime (Model model) =
    model.time


{-| Return whether VERBOSE is set in the server's environment
-}
verbose : Model servermodel message gamestate player -> Bool
verbose (Model model) =
    model.verbose


init : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Maybe String -> ( Model servermodel message gamestate player, Cmd Msg )
init servermodel userFunctions gamestate maybeVerbose =
    ( Model
        { servermodel = servermodel
        , userFunctions = userFunctions
        , state = emptyServerState gamestate
        , verbose = maybeVerbose /= Nothing
        , gameSocketsDict = Dict.empty
        , socketGamesDict = Dict.empty
        , playerSocketDict = Dict.empty
        , socketPlayersDict = Dict.empty
        , deathRowDuration = deathRowDuration
        , deathWatch = []
        , deathWatchGameids = Dict.empty
        , deathWatchPlayers = []
        , deathWatchPlayerids = Dict.empty
        , time = 0
        }
    , Task.perform FirstTick Time.now
    )



-- UPDATE


{-| The messages processed by our `update` function.

Opaque, because your code doesn't need to know.

-}
type Msg
    = Connection WSS.Socket
    | Disconnection WSS.Socket
    | SocketMessage Socket String
    | FirstTick Posix
    | Tick Posix
    | Noop



-- `Connection`, `Disconnection`, and `SocketMessage` come through the
-- `inputPort` from the Node.JS code.
-- `FirstTick` and `Tick` are used to track time. `Noop` does nothing.


update : Msg -> Model servermodel message gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )
update message model =
    let
        (Model mod) =
            model
    in
    case message of
        Connection socket ->
            ( model, Cmd.none )

        Disconnection socket ->
            disconnection model socket

        SocketMessage socket mess ->
            socketMessage model socket mess

        FirstTick time ->
            let
                state =
                    mod.state

                millis =
                    Time.posixToMillis time
            in
            ( Model
                { mod
                    | time = millis
                    , state =
                        { state
                            | seed =
                                Random.initialSeed millis
                        }
                }
            , Cmd.none
            )

        Tick time ->
            doExecutions <| Model { mod | time = Time.posixToMillis time }

        Noop ->
            ( model, Cmd.none )


killGame : Model servermodel message gamestate player -> GameId -> ( Model servermodel message gamestate player, Cmd Msg )
killGame model gameid =
    let
        (Model mdl) =
            model

        ( Model mdl2, cmd ) =
            case mdl.userFunctions.gamesDeleter of
                Nothing ->
                    ( model, Cmd.none )

                Just deleter ->
                    deleter model [ gameid ] mdl.state

        state =
            mdl2.state

        state2 =
            ServerInterface.removeGame gameid state
    in
    ( Model
        { mdl2
            | state =
                -- We don't just use state2 here, because we don't
                -- want the `changes` additions.
                { state
                    | dicts = state2.dicts
                    , publicGames = state2.publicGames
                }
        }
    , cmd
    )


killPlayer : Model servermodel message gamestate player -> GameId -> PlayerId -> ( Model servermodel message gamestate player, Cmd Msg )
killPlayer model gameid playerid =
    let
        (Model mdl) =
            model

        ( Model mdl2, cmd ) =
            case mdl.userFunctions.playersDeleter of
                Nothing ->
                    ( model, Cmd.none )

                Just deleter ->
                    deleter model gameid [ playerid ] mdl.state

        state =
            mdl2.state

        state2 =
            ServerInterface.removePlayer playerid state
    in
    ( Model
        { mdl2
            | state =
                -- We don't just use state2 here, because we don't
                -- want the `changes` additions.
                { state
                    | dicts = state2.dicts
                }
        }
    , cmd
    )


deathRowDuration : Int
deathRowDuration =
    30 * 1000


doExecutions : Model servermodel message gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )
doExecutions (Model mdl) =
    let
        time =
            mdl.time

        state =
            mdl.state

        model =
            { mdl
                | state =
                    { state | time = Time.millisToPosix time }
            }

        gameLoop ( mod, cmd ) watches =
            case watches of
                [] ->
                    ( mod, cmd )

                ( tim, gid ) :: tail ->
                    if time >= tim then
                        let
                            ( Model m, c ) =
                                killGame
                                    (Model
                                        { mod
                                            | deathWatch = tail
                                            , deathWatchGameids =
                                                Dict.remove gid
                                                    mod.deathWatchGameids
                                        }
                                    )
                                    gid
                        in
                        gameLoop
                            ( m
                            , Cmd.batch [ cmd, c ]
                            )
                            tail

                    else
                        ( mod, cmd )

        playerLoop ( mod, cmd ) watches =
            case watches of
                [] ->
                    ( Model mod, cmd )

                ( tim, gid, pid ) :: tail ->
                    if time >= tim then
                        let
                            ( Model m, c ) =
                                killPlayer
                                    (Model
                                        { mod
                                            | deathWatchPlayers = tail
                                            , deathWatchPlayerids =
                                                Dict.remove pid mod.deathWatchPlayerids
                                        }
                                    )
                                    gid
                                    pid
                        in
                        playerLoop
                            ( m
                            , Cmd.batch [ cmd, c ]
                            )
                            tail

                    else
                        ( Model mod, cmd )

        pair =
            gameLoop
                ( model
                , Cmd.none
                )
                model.deathWatch
    in
    playerLoop pair model.deathWatchPlayers


deathWatchGame : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatchGame gameid (Model model) =
    let
        doit =
            case model.userFunctions.autoDeleteGame of
                Nothing ->
                    True

                Just autoDeleteGame ->
                    autoDeleteGame gameid model.state

        gameids =
            model.deathWatchGameids
    in
    if not doit then
        Model model

    else
        case Dict.get gameid gameids of
            Just _ ->
                Model model

            Nothing ->
                Model
                    { model
                        | deathWatchGameids = Dict.insert gameid True gameids
                        , deathWatch =
                            List.append
                                model.deathWatch
                                [ ( model.time + model.deathRowDuration, gameid ) ]
                    }


reprieve : GameId -> Socket -> Model servermodel message gamestate player -> Model servermodel message gamestate player
reprieve gameid socket (Model model) =
    let
        gameids =
            model.deathWatchGameids
    in
    case Dict.get gameid gameids of
        Nothing ->
            Model model

        Just _ ->
            let
                sockets =
                    case Dict.get gameid model.gameSocketsDict of
                        Nothing ->
                            [ socket ]

                        Just socks ->
                            adjoin socket socks

                games =
                    case Dict.get socket model.socketGamesDict of
                        Nothing ->
                            [ gameid ]

                        Just gids ->
                            adjoin gameid gids
            in
            Model
                { model
                    | deathWatchGameids =
                        Dict.remove gameid gameids
                    , deathWatch =
                        List.filter (\( _, gid ) -> gid /= gameid) model.deathWatch
                    , gameSocketsDict =
                        Dict.insert gameid sockets model.gameSocketsDict
                    , socketGamesDict =
                        Dict.insert socket games model.socketGamesDict
                }


deathWatchPlayer : ( GameId, PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatchPlayer ( gameid, playerid ) (Model model) =
    let
        playerids =
            model.deathWatchPlayerids
    in
    case Dict.get playerid playerids of
        Just _ ->
            Model model

        Nothing ->
            Model
                { model
                    | deathWatchPlayerids = Dict.insert playerid True playerids
                    , deathWatchPlayers =
                        List.append
                            model.deathWatchPlayers
                            [ ( model.time + model.deathRowDuration, gameid, playerid ) ]
                }


adjoin : a -> List a -> List a
adjoin a list =
    if List.member a list then
        list

    else
        a :: list


reprievePlayer : PlayerId -> Socket -> Model servermodel message gamestate player -> Model servermodel message gamestate player
reprievePlayer playerid socket (Model model) =
    let
        playerids =
            model.deathWatchPlayerids
    in
    case Dict.get playerid playerids of
        Nothing ->
            Model model

        Just _ ->
            let
                mod =
                    case getPlayer playerid model.state of
                        Nothing ->
                            model

                        Just { gameid } ->
                            let
                                players =
                                    case Dict.get socket model.socketPlayersDict of
                                        Nothing ->
                                            [ ( gameid, playerid ) ]

                                        Just pids ->
                                            adjoin ( gameid, playerid ) pids

                                sockets =
                                    case Dict.get gameid model.gameSocketsDict of
                                        Nothing ->
                                            [ socket ]

                                        Just socks ->
                                            adjoin socket socks

                                games =
                                    case Dict.get socket model.socketGamesDict of
                                        Nothing ->
                                            [ gameid ]

                                        Just gids ->
                                            adjoin gameid gids
                            in
                            { model
                                | socketPlayersDict =
                                    Dict.insert socket players model.socketPlayersDict
                                , gameSocketsDict =
                                    Dict.insert gameid sockets model.gameSocketsDict
                                , socketGamesDict =
                                    Dict.insert socket games model.socketGamesDict
                            }
            in
            Model
                { mod
                    | deathWatchPlayerids =
                        Dict.remove playerid playerids
                    , deathWatchPlayers =
                        List.filter (\( _, _, pid ) -> pid /= playerid)
                            mod.deathWatchPlayers
                    , playerSocketDict =
                        Dict.insert playerid socket mod.playerSocketDict
                }


disconnection : Model servermodel message gamestate player -> Socket -> ( Model servermodel message gamestate player, Cmd Msg )
disconnection (Model model) socket =
    let
        folder pair mdl =
            let
                (Model m) =
                    deathWatchPlayer
                        pair
                    <|
                        Model
                            { mdl
                                | playerSocketDict =
                                    Dict.remove (Tuple.second pair)
                                        model.playerSocketDict
                            }
            in
            m

        doPlayer mdl =
            case Dict.get socket mdl.socketPlayersDict of
                Nothing ->
                    mdl

                Just playerPairs ->
                    List.foldl folder
                        mdl
                        playerPairs
    in
    case Dict.get socket model.socketGamesDict of
        Nothing ->
            ( Model <| doPlayer model
            , Cmd.none
            )

        Just gameids ->
            let
                dogame gameid model2 =
                    case Dict.get gameid model2.gameSocketsDict of
                        Nothing ->
                            model2

                        Just sockets ->
                            let
                                socks =
                                    List.filter ((/=) socket) sockets

                                model3 =
                                    { model2
                                        | gameSocketsDict =
                                            Dict.insert
                                                gameid
                                                socks
                                                model2.gameSocketsDict
                                    }
                            in
                            if socks == [] then
                                let
                                    (Model m) =
                                        deathWatchGame gameid (Model model3)
                                in
                                m

                            else
                                model3

                mdl =
                    List.foldl dogame model gameids

                mdl2 =
                    doPlayer mdl
            in
            ( Model
                { mdl2
                    | socketGamesDict =
                        Dict.remove socket model.socketGamesDict
                    , socketPlayersDict =
                        Dict.remove socket model.socketPlayersDict
                }
            , Cmd.none
            )


{-| Encode a message to a single socket via an output port.

If the first arg is True, log the operation on the console.

-}
sendToOne : MessageEncoder message -> message -> OutputPort Msg -> Socket -> Cmd Msg
sendToOne encoder message outputPort socket =
    WSS.sendToOne outputPort
        (encodeMessage encoder message)
        socket


{-| Encode a message to multiple sockets via an output port.
-}
sendToMany : MessageEncoder message -> message -> OutputPort Msg -> List Socket -> Cmd Msg
sendToMany encoder message outputPort sockets =
    WSS.sendToMany outputPort
        (encodeMessage encoder message)
        sockets
        |> Cmd.batch


{-| Encode a message to all the sockets for a GameId except the passed one.
-}
sendToOthers : GameId -> Socket -> Model servermodel message gamestate player -> MessageEncoder message -> message -> Cmd Msg
sendToOthers gameid socket model encoder message =
    let
        (Model mdl) =
            model

        outputPort =
            mdl.userFunctions.outputPort
    in
    sendToMany encoder message outputPort <|
        otherSockets gameid socket model


{-| Encode a message to all the sockets for a GameId.
-}
sendToAll : GameId -> Model servermodel message gamestate player -> MessageEncoder message -> message -> Cmd Msg
sendToAll gameid model encoder message =
    let
        (Model mdl) =
            model

        outputPort =
            mdl.userFunctions.outputPort
    in
    case Dict.get gameid mdl.gameSocketsDict of
        Nothing ->
            Cmd.none

        Just sockets ->
            sendToMany encoder message outputPort sockets


socketMessage : Model servermodel message gamestate player -> Socket -> String -> ( Model servermodel message gamestate player, Cmd Msg )
socketMessage (Model model) socket request =
    -- The `modeln` are wrapped. The `mdln` are unwrapped.
    let
        userFunctions =
            model.userFunctions
    in
    case decodeMessage userFunctions.encodeDecode.decoder request of
        (Err msg) as result ->
            case userFunctions.encodeDecode.errorWrapper of
                Nothing ->
                    ( Model model, Cmd.none )

                Just wrapper ->
                    let
                        response =
                            wrapper
                                { kind = JsonParseError
                                , description = request
                                , message = result
                                }
                    in
                    ( Model model
                    , sendToOne
                        userFunctions.encodeDecode.encoder
                        response
                        userFunctions.outputPort
                        socket
                    )

        Ok message ->
            let
                ( state, rsp ) =
                    userFunctions.messageProcessor model.state message

                (Model mdl) =
                    maybeReprieve message
                        socket
                        (Model { model | state = state })

                (Model mdl2) =
                    processAdds socket (Model mdl) mdl.state

                ( Model mdl3, cmd3 ) =
                    case rsp of
                        Nothing ->
                            ( Model mdl2
                            , Cmd.none
                            )

                        Just response ->
                            let
                                (Model mdl4) =
                                    maybeReprieve response socket (Model mdl2)
                            in
                            userFunctions.messageSender
                                (Model mdl4)
                                socket
                                mdl4.state
                                message
                                response

                mdl5 =
                    processRemoves socket (Model mdl3) mdl3.state
            in
            ( mdl5, cmd3 )


maybeReprieve : message -> Socket -> Model servermodel message gamestate player -> Model servermodel message gamestate player
maybeReprieve message socket (Model model) =
    let
        mod =
            case model.userFunctions.messageToGameid of
                Nothing ->
                    model

                Just messageToGameid ->
                    case messageToGameid message of
                        Nothing ->
                            model

                        Just gameid ->
                            let
                                (Model m) =
                                    reprieve gameid socket (Model model)
                            in
                            m
    in
    case model.userFunctions.messageToPlayerid of
        Nothing ->
            Model mod

        Just messageToPlayerid ->
            case messageToPlayerid message of
                Nothing ->
                    Model mod

                Just playerid ->
                    reprievePlayer playerid socket (Model mod)


{-| Return sockets associated with a game.

Removes the passed socket from the list.

Often useful in `ServerMessageSender` functions to send responses to all players.

-}
otherSockets : GameId -> Socket -> Model servermodel message gamestate player -> List Socket
otherSockets gameid socket (Model model) =
    case Dict.get gameid model.gameSocketsDict of
        Nothing ->
            []

        Just sockets ->
            LE.remove socket sockets


processAdds : Socket -> Model servermodel message gamestate player -> ServerState gamestate player -> Model servermodel message gamestate player
processAdds socket model state =
    case state.changes of
        Nothing ->
            model

        Just changes ->
            let
                model2 =
                    List.foldl (recordGameid socket)
                        model
                        changes.addedGames

                (Model mdl3) =
                    List.foldl (recordPlayerid socket)
                        model2
                        changes.addedPlayers
            in
            Model
                { mdl3
                    | state =
                        { state
                            | changes =
                                if
                                    (changes.removedGames == [])
                                        && (changes.removedPlayers == [])
                                then
                                    Nothing

                                else
                                    Just
                                        { changes
                                            | addedGames = []
                                            , addedPlayers = []
                                        }
                        }
                }


processRemoves : Socket -> Model servermodel message gamestate player -> ServerState gamestate player -> Model servermodel message gamestate player
processRemoves socket model state =
    case state.changes of
        Nothing ->
            model

        Just changes ->
            let
                model2 =
                    List.foldl removeGame
                        model
                        changes.removedGames

                (Model mdl3) =
                    List.foldl removePlayer
                        model2
                        changes.removedPlayers
            in
            Model
                { mdl3
                    | state =
                        { state
                            | changes =
                                if
                                    (changes.addedGames == [])
                                        && (changes.addedPlayers == [])
                                then
                                    Nothing

                                else
                                    Just
                                        { changes
                                            | removedGames = []
                                            , removedPlayers = []
                                        }
                        }
                }


recordGameid : Socket -> GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
recordGameid socket gameid (Model model) =
    Model
        { model
            | socketGamesDict =
                adjoinToSocketGamesDict gameid socket model.socketGamesDict
            , gameSocketsDict =
                let
                    gameSocketsDict =
                        model.gameSocketsDict

                    sockets =
                        case Dict.get gameid gameSocketsDict of
                            Nothing ->
                                [ socket ]

                            Just socks ->
                                adjoin socket socks
                in
                Dict.insert gameid sockets gameSocketsDict
        }


recordPlayerid : Socket -> ( GameId, PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
recordPlayerid socket ( gameid, playerid ) (Model model) =
    let
        sockets =
            case Dict.get gameid model.gameSocketsDict of
                Nothing ->
                    [ socket ]

                Just socks ->
                    if List.member socket socks then
                        socks

                    else
                        socket :: socks
    in
    Model
        { model
            | socketGamesDict =
                adjoinToSocketGamesDict gameid socket model.socketGamesDict
            , gameSocketsDict =
                Dict.insert gameid sockets model.gameSocketsDict
            , socketPlayersDict =
                adjoinToSocketPlayersDict gameid
                    playerid
                    socket
                    model.socketPlayersDict
            , playerSocketDict =
                Dict.insert playerid socket model.playerSocketDict
        }


adjoinToSocketGamesDict : GameId -> Socket -> Dict Socket (List GameId) -> Dict Socket (List GameId)
adjoinToSocketGamesDict gameid socket dict =
    let
        gids =
            case Dict.get socket dict of
                Nothing ->
                    [ gameid ]

                Just gameids ->
                    if List.member gameid gameids then
                        gameids

                    else
                        gameid :: gameids
    in
    Dict.insert socket gids dict


adjoinToSocketPlayersDict : GameId -> PlayerId -> Socket -> Dict Socket (List ( GameId, PlayerId )) -> Dict Socket (List ( GameId, PlayerId ))
adjoinToSocketPlayersDict gameid playerid socket dict =
    let
        pairs =
            case Dict.get socket dict of
                Nothing ->
                    [ ( gameid, playerid ) ]

                Just gids ->
                    ( gameid, playerid ) :: gids
    in
    Dict.insert socket pairs dict


removeGame : ( GameId, List PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
removeGame ( gameid, pids ) (Model model) =
    let
        mdl2 =
            let
                sockets =
                    case Dict.get gameid model.gameSocketsDict of
                        Nothing ->
                            []

                        Just socks ->
                            socks
            in
            { model
                | socketGamesDict =
                    List.foldl (removeFromSocketGamesDict gameid)
                        model.socketGamesDict
                        sockets
                , gameSocketsDict =
                    Dict.remove gameid model.gameSocketsDict
                , socketPlayersDict =
                    List.foldl Dict.remove
                        model.socketPlayersDict
                        sockets
            }
    in
    Model
        { mdl2
            | playerSocketDict =
                List.foldl Dict.remove model.playerSocketDict pids
        }


removeFromSocketGamesDict : GameId -> Socket -> Dict Socket (List GameId) -> Dict Socket (List GameId)
removeFromSocketGamesDict gameid socket dict =
    case Dict.get socket dict of
        Nothing ->
            dict

        Just gids ->
            case LE.remove gameid gids of
                [] ->
                    Dict.remove socket dict

                gids2 ->
                    Dict.insert socket gids2 dict


removePlayer : ( GameId, PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
removePlayer ( gameid, playerid ) (Model model) =
    let
        mdl =
            case Dict.get playerid model.playerSocketDict of
                Nothing ->
                    model

                Just socket ->
                    { model
                        | socketPlayersDict =
                            Dict.remove socket
                                model.socketPlayersDict
                    }
    in
    Model
        { mdl
            | playerSocketDict =
                Dict.remove playerid model.playerSocketDict
        }



-- SUBSCRIPTIONS


decodeMsg : JD.Value -> Msg
decodeMsg value =
    let
        decoder =
            WSS.eventDecoder
                { onConnection = \socket _ -> Connection socket
                , onDisconnection = \socket _ -> Disconnection socket
                , onMessage = \socket _ message -> SocketMessage socket message
                }
    in
    JD.decodeValue decoder value
        |> Result.withDefault Noop


subscriptions : InputPort Msg -> Model servermodel message gamestate player -> Sub Msg
subscriptions inputPort model =
    Sub.batch
        [ inputPort decodeMsg
        , Time.every 1000 Tick
        ]
