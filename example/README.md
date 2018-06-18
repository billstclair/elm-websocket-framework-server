This directory contains the server side of an example of using the `WebSocketFramework` module.

`ExampleInterface.elm` is a copy of the same file in the example directory of the [billstclair/elm-websocket-framework](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework-server/latest) package.

`Server.elm` is the top-level program for the server. It's a very simple protocol, so there's not much to it.

To build and run the server, see the README in the [server](server/) directory.

The `server` directory is separate from the example code, because that directory is usable as is for ANY server that names its top-level module `Server`.

