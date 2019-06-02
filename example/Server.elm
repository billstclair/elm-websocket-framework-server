port module Server exposing (ServerModel, encodeDecode, errorWrapper, inputPort, main, messageSender, outputPort, serverModel, userFunctions)

import ExampleInterface
    exposing
        ( GameState
        , Message(..)
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )
import WebSocketFramework.Server
    exposing
        ( ServerMessageSender
        , UserFunctions
        , program
        , sendToOne
        , verbose
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , InputPort
        , OutputPort
        , ServerState
        )


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


tos : Int -> String
tos x =
    String.fromInt x


errorWrapper : Error Message -> Message
errorWrapper { kind, description, message } =
    case kind of
        JsonParseError ->
            let
                err =
                    case message of
                        Err msg ->
                            msg

                        Ok msg ->
                            Debug.toString msg
            in
            ErrorMessage
                { request = description
                , error = "JSON parser error: " ++ err
                }

        _ ->
            ErrorMessage
                { request = ""
                , error = Debug.toString message
                }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Just errorWrapper
    }


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    ( model, sendToOne (verbose model) messageEncoder response outputPort socket )


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Nothing
    , messageToPlayerid = Nothing
    , autoDeleteGame = Nothing
    , gamesDeleter = Nothing
    , playersDeleter = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
