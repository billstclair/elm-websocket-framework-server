port module Server exposing (..)

import ExampleInterface
    exposing
        ( GameState
        , Message
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , UserFunctions
        , WrappedModel
        , program
        , sendToOne
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , InputPort
        , OutputPort
        , ServerState
        )


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Nothing
    }


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    ( model, sendToOne messageEncoder response outputPort socket )


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
