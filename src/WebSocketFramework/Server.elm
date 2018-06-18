module WebSocketFramework.Server
    exposing
        ( Model
        , Msg(..)
        , ServerMessageSender
        , UserFunctions
        , WrappedModel(WrappedModel)
        , init
        , newGameid
        , newPlayerid
        , program
        , sendToMany
        , sendToOne
        )

{-| Support for a Node.js server for WebSocketFramework messages.


# Types

@docs Model, Msg, ServerMessageSender, UserFunctions, WrappedModel


# Top-level program

@docs program, init


# Message sending

@docs sendToOne, sendToMany


# Support for creating random game and player identifiers.

@docs newGameid, newPlayerid

-}

import Char
import Debug exposing (log)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra as LE
import Platform exposing (Program)
import Random exposing (Generator, Seed)
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
import WebSocketServer as WSS exposing (Socket)


{-| Create the top-level application program.

You will usually use the result of this function as the value of `main` in your top-level module.

Most servers will not need to use the `servermodel`, but it's a place to stash extra server-wide state that doesn't make sense in the game-specific `gamestate`.

-}
program : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Program Never (Model servermodel message gamestate player) Msg
program servermodel userFunctions gamestate =
    Platform.program
        { init = init servermodel userFunctions gamestate
        , update = update
        , subscriptions = subscriptions userFunctions.inputPort
        }



-- MODEL


type alias DeathWatch =
    ( Time, GameId )


type alias DeathWatchGameids =
    Dict GameId Bool


{-| User function that is called to send the response(s) to a request.

This will usually call `sentToOne` and/or `sendToMany` with the `message` emitted by the `ServiceMessageProcessor` in the `UserFunctions` passed to `program`.

The first `message` is the request that came from client to server. The second `message` is the response. If no response is returned by the `ServiceMessageProcessor`, this function is not called.

-}
type alias ServerMessageSender servermodel message gamestate player =
    WrappedModel servermodel message gamestate player -> Socket -> ServerState gamestate player -> message -> message -> ( WrappedModel servermodel message gamestate player, Cmd Msg )


{-| A type wrapper to prevent recursive types in `Model`.
-}
type WrappedModel servermodel message gamestate player
    = WrappedModel (Model servermodel message gamestate player)


{-| User functions that get called by the generic server code.

`encodeDecode` is used to translate messages to and from strings.

`messageProcessor` processes a client request into state changes and a response message.

`messageSender` decides what to do with the response message.

`messageToGameid` extracts a GameId from a message, if there is one.

`inputPort` and `outputPort` are the ports used to communicate with the Node.js code.

-}
type alias UserFunctions servermodel message gamestate player =
    { encodeDecode : EncodeDecode message
    , messageProcessor : ServerMessageProcessor gamestate player message
    , messageSender : ServerMessageSender servermodel message gamestate player
    , messageToGameid : Maybe (MessageToGameid message)
    , inputPort : InputPort Msg
    , outputPort : OutputPort Msg
    }


{-| The server application model.

`program` creates one of these, via a call to `init`.

-}
type alias Model servermodel message gamestate player =
    { servermodel : servermodel
    , userFunctions : UserFunctions servermodel message gamestate player
    , state : ServerState gamestate player
    , gameidDict : Dict Socket GameId
    , playeridDict : Dict GameId (List PlayerId)
    , socketsDict : Dict GameId (List Socket)
    , deathWatch : List DeathWatch
    , deathWatchGameids : DeathWatchGameids
    , time : Time
    , seed : Seed
    }


{-| Return the initial `Model` and a `Cmd` to get the current time.

Usually called for you by `program`.

-}
init : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> ( Model servermodel message gamestate player, Cmd Msg )
init servermodel userFunctions gamestate =
    ( { servermodel = servermodel
      , userFunctions = userFunctions
      , state = emptyServerState gamestate
      , gameidDict = Dict.empty
      , playeridDict = Dict.empty
      , socketsDict = Dict.empty
      , deathWatch = []
      , deathWatchGameids = Dict.empty
      , time = 0
      , seed = Random.initialSeed 0
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


maybeLog : Msg -> Msg
maybeLog msg =
    case msg of
        Tick _ ->
            msg

        x ->
            log "Msg" x


update : Msg -> Model servermodel message gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )
update message model =
    case maybeLog message of
        Connection socket ->
            ( model, Cmd.none )

        Disconnection socket ->
            disconnection model socket

        SocketMessage socket message ->
            socketMessage model socket message

        FirstTick time ->
            let
                seed =
                    Random.initialSeed <| round time
            in
            ( { model
                | time = time
                , seed = seed
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
        state =
            model.state

        playerDict =
            case Dict.get (log "killgame" gameid) model.playeridDict of
                Nothing ->
                    state.playerDict

                Just ids ->
                    List.foldl Dict.remove state.playerDict ids
    in
    { model
        | state =
            { state
                | gameDict = Dict.remove gameid state.gameDict
                , playerDict = playerDict
                , publicGames = removeField gameid .gameid state.publicGames
            }
        , playeridDict = Dict.remove gameid model.playeridDict
    }


deathRowDuration : Time
deathRowDuration =
    2 * Time.minute


doExecutions : Model servermodel message gamestate player -> Model servermodel message gamestate player
doExecutions model =
    let
        time =
            model.time

        loop =
            \mod watches ->
                case watches of
                    [] ->
                        mod

                    ( tim, gid ) :: tail ->
                        if time >= tim then
                            loop
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
    in
    loop model model.deathWatch


deathWatch : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatch gameid model =
    let
        gameids =
            model.deathWatchGameids
    in
    case Dict.get (log "deathWatch" gameid) gameids of
        Just _ ->
            model

        Nothing ->
            { model
                | deathWatchGameids = Dict.insert gameid True gameids
                , deathWatch =
                    List.append
                        model.deathWatch
                        [ ( model.time + deathRowDuration, gameid ) ]
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
                | deathWatchGameids = Dict.remove (log "reprieve" gameid) gameids
                , deathWatch =
                    List.filter (\( _, gid ) -> gid /= gameid) model.deathWatch
            }


disconnection : Model servermodel message gamestate player -> Socket -> ( Model servermodel message gamestate player, Cmd Msg )
disconnection model socket =
    case Dict.get socket model.gameidDict of
        Nothing ->
            ( model, Cmd.none )

        Just gameid ->
            let
                model2 =
                    { model | gameidDict = Dict.remove socket model.gameidDict }

                socketsDict =
                    model.socketsDict
            in
            case Dict.get gameid model.socketsDict of
                Nothing ->
                    ( model2, Cmd.none )

                Just sockets ->
                    let
                        socks =
                            List.filter (\s -> s /= socket) sockets

                        model3 =
                            { model2
                                | socketsDict =
                                    Dict.insert gameid socks socketsDict
                            }
                    in
                    ( if socks == [] then
                        deathWatch gameid model3
                      else
                        model3
                    , Cmd.none
                    )


{-| Send a message to a single socket.
-}
sendToOne : MessageEncoder message -> message -> OutputPort Msg -> Socket -> Cmd Msg
sendToOne encoder message outputPort socket =
    WSS.sendToOne outputPort
        (log "send" <| encodeMessage encoder message)
        (log "  " socket)


{-| Send a message to a multiple sockets.
-}
sendToMany : MessageEncoder message -> message -> OutputPort Msg -> List Socket -> Cmd Msg
sendToMany encoder message outputPort sockets =
    WSS.sendToMany outputPort
        (log "send" (encodeMessage encoder message))
        (log "  " sockets)
        |> Cmd.batch


socketMessage : Model servermodel message gamestate player -> Socket -> String -> ( Model servermodel message gamestate player, Cmd Msg )
socketMessage model socket request =
    let
        userFunctions =
            model.userFunctions
    in
    case decodeMessage userFunctions.encodeDecode.decoder request of
        Err msg ->
            case userFunctions.encodeDecode.errorWrapper of
                Nothing ->
                    ( model, Cmd.none )

                Just wrapper ->
                    let
                        response =
                            wrapper <| "Can't parse request: " ++ request
                    in
                    ( model
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
            in
            case rsp of
                Nothing ->
                    ( { model | state = state }
                    , Cmd.none
                    )

                Just response ->
                    let
                        mod =
                            case userFunctions.messageToGameid of
                                Nothing ->
                                    model

                                Just messageGameid ->
                                    case messageGameid response of
                                        Nothing ->
                                            model

                                        Just gameid ->
                                            reprieve gameid model

                        ( WrappedModel mod2, cmd ) =
                            userFunctions.messageSender
                                (WrappedModel mod)
                                socket
                                state
                                message
                                response
                    in
                    ( mod2, cmd )


lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


{-| (log (expt 26 16) 2) -> 75
-}
gameidLength : Int
gameidLength =
    16


gameidGenerator : Generator GameId
gameidGenerator =
    Random.map String.fromList <|
        Random.list gameidLength lowercaseLetter


{-| Generate a random GameId string, ensuring that it is not already assigned to a game.

Will not be used by servers that have no concept of a game.

-}
newGameid : WrappedModel servermodel message gamestate player -> ( GameId, WrappedModel servermodel message gamestate player )
newGameid (WrappedModel model) =
    let
        ( res, seed ) =
            Random.step gameidGenerator model.seed

        mdl2 =
            WrappedModel { model | seed = seed }
    in
    case Dict.get res model.state.gameDict of
        Nothing ->
            ( res, mdl2 )

        Just _ ->
            newGameid mdl2


{-| Generate a random PlayerId string, ensuring that it is not already assigned to a player.

Will not be used by servers that have no concept of a player.

-}
newPlayerid : WrappedModel servermodel message gamestate player -> ( PlayerId, WrappedModel servermodel message gamestate player )
newPlayerid model =
    let
        ( gameid, mod ) =
            newGameid model

        playerid =
            "P" ++ gameid
    in
    case mod of
        WrappedModel mdl ->
            case Dict.get playerid mdl.state.playerDict of
                Nothing ->
                    ( playerid, mod )

                Just _ ->
                    newPlayerid mod



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
