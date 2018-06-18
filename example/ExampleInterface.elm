---------------------------------------------------------------------
--
-- ExampleInterface.elm
-- Processor and encoders/decoders for WebSocketFramework example
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ExampleInterface
    exposing
        ( GameState
        , Message(..)
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )

import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.Types exposing (Plist, ReqRsp(..), ServerState)


type alias GameState =
    ()


type alias Player =
    String


type Message
    = AddMessage Int Int
    | MultiplyMessage Int Int
    | ResultMessage String


messageProcessor : ServerState GameState Player -> Message -> ( ServerState GameState Player, Maybe Message )
messageProcessor state message =
    case message of
        AddMessage x y ->
            ( state
            , Just
                (ResultMessage <|
                    toString x
                        ++ " + "
                        ++ toString y
                        ++ " = "
                        ++ toString (x + y)
                )
            )

        MultiplyMessage x y ->
            ( state
            , Just
                (ResultMessage <|
                    toString x
                        ++ " * "
                        ++ toString y
                        ++ " = "
                        ++ toString (x * y)
                )
            )

        _ ->
            --should really send an error, but ignore for now
            ( state, Nothing )



---
--- Encoder and parser not used until I do the real server, but here they are.
---


messageEncoder : Message -> ( ReqRsp, Plist )
messageEncoder message =
    case message of
        AddMessage x y ->
            ( Req "add"
            , [ ( "x", JE.int x )
              , ( "y", JE.int y )
              ]
            )

        MultiplyMessage x y ->
            ( Req "multiply"
            , [ ( "x", JE.int x )
              , ( "y", JE.int y )
              ]
            )

        ResultMessage result ->
            ( Rsp "result"
            , [ ( "result", JE.string result )
              ]
            )


messageDecoder : ( ReqRsp, Plist ) -> Result String Message
messageDecoder ( reqrsp, plist ) =
    case reqrsp of
        Req msg ->
            case msg of
                "add" ->
                    decodePlist addDecoder plist

                "multiply" ->
                    decodePlist multiplyDecoder plist

                _ ->
                    unknownMessage reqrsp

        Rsp msg ->
            case msg of
                "result" ->
                    decodePlist resultDecoder plist

                _ ->
                    unknownMessage reqrsp


addDecoder : Decoder Message
addDecoder =
    xyDecoder AddMessage


multiplyDecoder : Decoder Message
multiplyDecoder =
    xyDecoder MultiplyMessage


xyDecoder : (Int -> Int -> Message) -> Decoder Message
xyDecoder constructor =
    JD.map2 constructor
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)


resultDecoder : Decoder Message
resultDecoder =
    JD.map ResultMessage
        (JD.field "result" JD.string)
