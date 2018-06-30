module WebSocketFramework.Server
    exposing
        ( Model
        , Msg(..)
        , ServerGamesDeleter
        , ServerMessageSender
        , ServerPlayersDeleter
        , Socket
        , UserFunctions
        , WrappedModel(WrappedModel)
        , init
        , otherSockets
        , program
        , sendToMany
        , sendToOne
        , verbose
        )

{-| Support for a Node.js server for WebSocketFramework messages.


# Types

@docs Model, Msg, WrappedModel, Socket


# Callbacks

@docs UserFunctions, ServerMessageSender, ServerGamesDeleter, ServerPlayersDeleter


# Top-level program

@docs program, init


# Message sending

@docs sendToOne, sendToMany


# Utilities

@docs otherSockets


# Model accessors

@docs verbose

-}

import Char
import Debug exposing (log)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra as LE
import Platform exposing (Program)
import Task
import Time exposing (Time)
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.ServerInterface
    exposing
        ( errorRsp
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

Most servers will not need to use the `servermodel`, but it's a place to stash extra server-wide state that doesn't make sense in the game-specific `gamestate`.

-}
program : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Program (Maybe String) (Model servermodel message gamestate player) Msg
program servermodel userFunctions gamestate =
    Platform.programWithFlags
        { init = init servermodel userFunctions gamestate
        , update = update
        , subscriptions = subscriptions userFunctions.inputPort
        }



-- MODEL


{-| User function that is called to send the response(s) to a request.

This will usually call `sendToOne` and/or `sendToMany` with the `message` emitted by the `ServiceMessageProcessor` in the `UserFunctions` passed to `program`.

The first `message` is the request that came from client to server. The second `message` is the response. If no response is returned by the `ServiceMessageProcessor`, this function is not called.

The `ServerState` arg is the value of the `state` property of the `WrappedModel` arg, pulled out for your convenience. If you change it, you must put it back in the model you return.

-}
type alias ServerMessageSender servermodel message gamestate player =
    WrappedModel servermodel message gamestate player -> Socket -> ServerState gamestate player -> message -> message -> ( WrappedModel servermodel message gamestate player, Cmd Msg )


{-| Called when games are auto-deleted due to socket connections being lost.

This will only happen if your server code tracks the association between sockets, games and players in the `xxxDict` properties of the Model. When tracked games are dropped, this function, stored in the `gamesDeleter` property of `UserFunctions`, is called, so that you can clean up any reference to those games in your `gamestate`.

-}
type alias ServerGamesDeleter servermodel message gamestate player =
    WrappedModel servermodel message gamestate player -> List GameId -> ServerState gamestate player -> WrappedModel servermodel message gamestate player


{-| Called when players are auto-deleted due to socket connections being lost.

This will only happen if your server code tracks the association between sockets, games and players in the `xxxDict` properties of the Model. When tracked players are dropped, this function, stored in the `playersDelete` property of `UserFunctions`, is called, so that you can clean up any reference to those players in your `gamestate`.

-}
type alias ServerPlayersDeleter servermodel message gamestate player =
    WrappedModel servermodel message gamestate player -> List PlayerId -> ServerState gamestate player -> WrappedModel servermodel message gamestate player


{-| A type wrapper to prevent recursive types in `Model`.
-}
type WrappedModel servermodel message gamestate player
    = WrappedModel (Model servermodel message gamestate player)


{-| An alias of WebSocketServer.Socket.
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


{-| The server application model.

`program` creates one of these, via a call to `init`.

The four `Dict`s, mapping games, players, and sockets to each other, are used to give some time for a lost connection to come back. You must call the functions that maintain them, if you want those features.

`deathRowDuration` is the time that a socket must remain disconnected before its games and players will be auto-removed. It defaults to 30 seconds.

`verbose` is true to enable console logging of messages received and sent and auto-deletions. It is usually inititalized by the VERBOSE environment variable at server start time.

The `deathWatch` variables are used for the auto-removal process. You shouldn't need to deal with them, other than providing `messageToGameid` and `messageToPlayerid` values for `userFunctions`.

-}
type alias Model servermodel message gamestate player =
    { servermodel : servermodel
    , userFunctions : UserFunctions servermodel message gamestate player
    , state : ServerState gamestate player
    , verbose : Bool
    , gamePlayersDict : Dict GameId (List PlayerId)
    , gameSocketsDict : Dict GameId (List Socket)
    , socketGamesDict : Dict Socket (List GameId)
    , playerSocketDict : Dict PlayerId Socket
    , socketPlayersDict : Dict Socket (List ( GameId, PlayerId ))
    , deathRowDuration : Time
    , deathWatch : List ( Time, GameId )
    , deathWatchGameids : Dict GameId Bool
    , deathWatchPlayers : List ( Time, GameId, PlayerId )
    , deathWatchPlayerids : Dict PlayerId Bool
    , time : Time
    }


{-| Return whether VERBOSE is set in the server's environment
-}
verbose : WrappedModel servermodel message gamestate player -> Bool
verbose (WrappedModel model) =
    model.verbose


{-| Return the initial `Model` and a `Cmd` to get the current time.

Usually called for you by `program`.

-}
init : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Maybe String -> ( Model servermodel message gamestate player, Cmd Msg )
init servermodel userFunctions gamestate verbose =
    ( { servermodel = servermodel
      , userFunctions = userFunctions
      , state = emptyServerState gamestate
      , verbose = verbose /= Nothing
      , gamePlayersDict = Dict.empty
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

`Connection`, `Disconnection`, and `SocketMessage` from through the `inputPort` from the Node.JS code. `FirstTick` and `Tick` are used to track time. `Noop` does nothing.

-}
type Msg
    = Connection WSS.Socket
    | Disconnection WSS.Socket
    | SocketMessage Socket String
    | FirstTick Time
    | Tick Time
    | Noop


maybeLog : Bool -> String -> x -> x
maybeLog verbose label x =
    if verbose then
        log label x
    else
        x


maybeLogMsg : Bool -> Msg -> Msg
maybeLogMsg verbose msg =
    case msg of
        Tick _ ->
            msg

        x ->
            maybeLog verbose "Msg" x


update : Msg -> Model servermodel message gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )
update message model =
    case maybeLogMsg model.verbose message of
        Connection socket ->
            ( model, Cmd.none )

        Disconnection socket ->
            disconnection model socket

        SocketMessage socket message ->
            socketMessage model socket message

        FirstTick time ->
            ( { model
                | time = time
              }
            , Cmd.none
            )

        Tick time ->
            ( doExecutions { model | time = time }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )


removeField : value -> (record -> value) -> List record -> List record
removeField value accessor records =
    List.filter (\record -> accessor record /= value) records


killGame : Model servermodel message gamestate player -> GameId -> Model servermodel message gamestate player
killGame model gameid =
    let
        (WrappedModel mdl) =
            case model.userFunctions.gamesDeleter of
                Nothing ->
                    WrappedModel model

                Just deleter ->
                    deleter (WrappedModel model) [ gameid ] model.state

        state =
            mdl.state

        playerDict =
            case
                Dict.get (maybeLog model.verbose "killgame" gameid)
                    model.gamePlayersDict
            of
                Nothing ->
                    state.playerDict

                Just ids ->
                    List.foldl Dict.remove state.playerDict ids
    in
    { mdl
        | state =
            { state
                | gameDict = Dict.remove gameid state.gameDict
                , playerDict = playerDict
                , publicGames = removeField gameid .gameid state.publicGames
            }
        , gamePlayersDict = Dict.remove gameid model.gamePlayersDict
    }


killPlayer : Model servermodel message gamestate player -> GameId -> PlayerId -> Model servermodel message gamestate player
killPlayer model gameid playerid =
    let
        (WrappedModel mdl) =
            case model.userFunctions.playersDeleter of
                Nothing ->
                    WrappedModel model

                Just deleter ->
                    deleter (WrappedModel model) [ playerid ] model.state

        state =
            mdl.state

        gamePlayersDict =
            case Dict.get gameid mdl.gamePlayersDict of
                Nothing ->
                    mdl.gamePlayersDict

                Just playerids ->
                    case LE.remove playerid playerids of
                        [] ->
                            Dict.remove playerid mdl.gamePlayersDict

                        pids ->
                            Dict.insert playerid pids mdl.gamePlayersDict
    in
    { mdl
        | state =
            { state
                | playerDict =
                    Dict.remove
                        (maybeLog mdl.verbose "killPlayer" playerid)
                        state.playerDict
            }
        , gamePlayersDict = gamePlayersDict
    }


deathRowDuration : Time
deathRowDuration =
    30 * Time.second


doExecutions : Model servermodel message gamestate player -> Model servermodel message gamestate player
doExecutions model =
    let
        time =
            model.time

        gameLoop =
            \mod watches ->
                case watches of
                    [] ->
                        mod

                    ( tim, gid ) :: tail ->
                        if time >= tim then
                            gameLoop
                                (killGame
                                    { mod
                                        | deathWatch = tail
                                        , deathWatchGameids =
                                            Dict.remove gid mod.deathWatchGameids
                                    }
                                    gid
                                )
                                tail
                        else
                            mod

        playerLoop =
            \mod watches ->
                case watches of
                    [] ->
                        mod

                    ( tim, gid, pid ) :: tail ->
                        if time >= tim then
                            playerLoop
                                (killPlayer
                                    { mod
                                        | deathWatchPlayers = tail
                                        , deathWatchPlayerids =
                                            Dict.remove pid mod.deathWatchPlayerids
                                    }
                                    gid
                                    pid
                                )
                                tail
                        else
                            mod

        mdl =
            gameLoop model model.deathWatch
    in
    playerLoop mdl model.deathWatchPlayers


deathWatchGame : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatchGame gameid model =
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
        model
    else
        case Dict.get (maybeLog model.verbose "deathWatch" gameid) gameids of
            Just _ ->
                model

            Nothing ->
                { model
                    | deathWatchGameids = Dict.insert gameid True gameids
                    , deathWatch =
                        List.append
                            model.deathWatch
                            [ ( model.time + model.deathRowDuration, gameid ) ]
                }


reprieve : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
reprieve gameid model =
    let
        gameids =
            model.deathWatchGameids
    in
    case Dict.get gameid gameids of
        Nothing ->
            model

        Just _ ->
            { model
                | deathWatchGameids =
                    Dict.remove (maybeLog model.verbose "reprieve" gameid) gameids
                , deathWatch =
                    List.filter (\( _, gid ) -> gid /= gameid) model.deathWatch
            }


deathWatchPlayer : ( GameId, PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatchPlayer ( gameid, playerid ) model =
    let
        playerids =
            model.deathWatchPlayerids
    in
    case Dict.get (maybeLog model.verbose "deathWatchPlayer" playerid) playerids of
        Just _ ->
            model

        Nothing ->
            { model
                | deathWatchPlayerids = Dict.insert playerid True playerids
                , deathWatchPlayers =
                    List.append
                        model.deathWatchPlayers
                        [ ( model.time + model.deathRowDuration, gameid, playerid ) ]
            }


reprievePlayer : PlayerId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
reprievePlayer playerid model =
    let
        playerids =
            model.deathWatchPlayerids
    in
    case Dict.get playerid playerids of
        Nothing ->
            model

        Just _ ->
            { model
                | deathWatchPlayerids =
                    Dict.remove (maybeLog model.verbose "reprievePlayer" playerid) playerids
                , deathWatchPlayers =
                    List.filter (\( _, _, pid ) -> pid /= playerid)
                        model.deathWatchPlayers
            }


disconnection : Model servermodel message gamestate player -> Socket -> ( Model servermodel message gamestate player, Cmd Msg )
disconnection model socket =
    let
        doPlayer =
            \model ->
                case Dict.get socket model.socketPlayersDict of
                    Nothing ->
                        model

                    Just playerPairs ->
                        List.foldl
                            (\pair model ->
                                deathWatchPlayer
                                    pair
                                    { model
                                        | playerSocketDict =
                                            Dict.remove (Tuple.second pair)
                                                model.playerSocketDict
                                    }
                            )
                            model
                            playerPairs
    in
    case Dict.get socket model.socketGamesDict of
        Nothing ->
            doPlayer model ! []

        Just gameids ->
            let
                dogame =
                    \gameid model ->
                        case Dict.get gameid model.gameSocketsDict of
                            Nothing ->
                                model

                            Just sockets ->
                                let
                                    socks =
                                        List.filter ((/=) socket) sockets

                                    model2 =
                                        { model
                                            | gameSocketsDict =
                                                Dict.insert
                                                    gameid
                                                    socks
                                                    model.gameSocketsDict
                                        }
                                in
                                if socks == [] then
                                    deathWatchGame gameid model2
                                else
                                    model2

                mdl =
                    List.foldl dogame model gameids

                mdl2 =
                    doPlayer mdl
            in
            { mdl2
                | socketGamesDict =
                    Dict.remove socket model.socketGamesDict
                , socketPlayersDict =
                    Dict.remove socket model.socketPlayersDict
            }
                ! []


{-| Encode a message to a single socket via an output port.

If the first arg is True, log the operation on the console.

-}
sendToOne : Bool -> MessageEncoder message -> message -> OutputPort Msg -> Socket -> Cmd Msg
sendToOne verbose encoder message outputPort socket =
    WSS.sendToOne outputPort
        (maybeLog verbose "sendToOne" <| encodeMessage encoder message)
        (maybeLog verbose "  " socket)


{-| Encode a message to multiple sockets via an output port.

If the first arg is True, log the operation on the console.

-}
sendToMany : Bool -> MessageEncoder message -> message -> OutputPort Msg -> List Socket -> Cmd Msg
sendToMany verbose encoder message outputPort sockets =
    WSS.sendToMany outputPort
        (maybeLog verbose "sendToMany" <| encodeMessage encoder message)
        (maybeLog verbose "  " sockets)
        |> Cmd.batch


socketMessage : Model servermodel message gamestate player -> Socket -> String -> ( Model servermodel message gamestate player, Cmd Msg )
socketMessage model socket request =
    let
        userFunctions =
            model.userFunctions
    in
    case decodeMessage userFunctions.encodeDecode.decoder request of
        (Err msg) as result ->
            case userFunctions.encodeDecode.errorWrapper of
                Nothing ->
                    ( model, Cmd.none )

                Just wrapper ->
                    let
                        response =
                            wrapper
                                { kind = JsonParseError
                                , description = request
                                , message = result
                                }
                    in
                    ( model
                    , sendToOne
                        model.verbose
                        userFunctions.encodeDecode.encoder
                        response
                        userFunctions.outputPort
                        socket
                    )

        Ok message ->
            let
                ( state, rsp ) =
                    userFunctions.messageProcessor model.state message

                mdl =
                    processAddsAndRemoves socket model state

                mod =
                    maybeReprieve message mdl
            in
            case rsp of
                Nothing ->
                    mod ! []

                Just response ->
                    let
                        mod2 =
                            maybeReprieve response mod

                        ( WrappedModel mod3, cmd ) =
                            userFunctions.messageSender
                                (WrappedModel mod2)
                                socket
                                state
                                message
                                response
                    in
                    ( mod3, cmd )


maybeReprieve : message -> Model servermodel message gamestate player -> Model servermodel message gamestate player
maybeReprieve message model =
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
                            reprieve gameid model
    in
    case model.userFunctions.messageToPlayerid of
        Nothing ->
            mod

        Just messageToPlayerid ->
            case messageToPlayerid message of
                Nothing ->
                    mod

                Just playerid ->
                    reprievePlayer playerid mod


{-| Return sockets associated with a game.

Removes the passed socket from the list.

Often useful in `ServerMessageSender` functions to send responses to all players.

-}
otherSockets : GameId -> Socket -> WrappedModel servermodel message gamestate player -> List Socket
otherSockets gameid socket (WrappedModel model) =
    case Dict.get gameid model.gameSocketsDict of
        Nothing ->
            []

        Just sockets ->
            LE.remove socket sockets


processAddsAndRemoves : Socket -> Model servermodel message gamestate player -> ServerState gamestate player -> Model servermodel message gamestate player
processAddsAndRemoves socket model state =
    case state.changes of
        Nothing ->
            model

        Just changes ->
            let
                mdl =
                    List.foldl (recordGameid socket)
                        model
                        changes.addedGames

                mdl2 =
                    List.foldl (recordPlayerid socket)
                        mdl
                        changes.addedPlayers

                mdl3 =
                    List.foldl removeGame
                        mdl2
                        changes.removedGames

                mdl4 =
                    List.foldl removePlayer
                        mdl3
                        changes.removedPlayers
            in
            { mdl4
                | state =
                    { state | changes = Nothing }
            }


recordGameid : Socket -> GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
recordGameid socket gameid model =
    { model
        | socketGamesDict =
            adjoinToSocketGamesDict gameid socket model.socketGamesDict
        , gameSocketsDict =
            Dict.insert gameid [ socket ] model.gameSocketsDict
    }


recordPlayerid : Socket -> ( GameId, PlayerId ) -> Model servermodel message gamestate player -> Model servermodel message gamestate player
recordPlayerid socket ( gameid, playerid ) model =
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

        playerids =
            case Dict.get gameid model.gamePlayersDict of
                Nothing ->
                    [ playerid ]

                Just pids ->
                    playerid :: pids
    in
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
        , gamePlayersDict =
            Dict.insert gameid playerids model.gamePlayersDict
    }


adjoinToSocketGamesDict : GameId -> Socket -> Dict Socket (List GameId) -> Dict Socket (List GameId)
adjoinToSocketGamesDict gameid socket dict =
    let
        gids =
            case Dict.get socket dict of
                Nothing ->
                    [ gameid ]

                Just gids ->
                    if List.member gameid gids then
                        gids
                    else
                        gameid :: gids
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


removeGame : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
removeGame gameid model =
    let
        mdl2 =
            let
                sockets =
                    case Dict.get gameid model.gameSocketsDict of
                        Nothing ->
                            []

                        Just sockets ->
                            sockets
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

        mdl3 =
            case Dict.get gameid mdl2.gamePlayersDict of
                Nothing ->
                    mdl2

                Just players ->
                    { mdl2
                        | playerSocketDict =
                            List.foldl Dict.remove
                                model.playerSocketDict
                                players
                    }
    in
    { mdl3
        | gamePlayersDict = Dict.remove gameid mdl2.gamePlayersDict
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
removePlayer ( gameid, playerid ) model =
    case Dict.get gameid model.gamePlayersDict of
        Nothing ->
            model

        Just playerids ->
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
            { mdl
                | gamePlayersDict =
                    case LE.remove playerid playerids of
                        [] ->
                            Dict.remove gameid model.gamePlayersDict

                        playerids ->
                            Dict.insert gameid playerids model.gamePlayersDict
                , playerSocketDict =
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
        , Time.every Time.second Tick
        ]
