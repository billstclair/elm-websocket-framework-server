var port = (process.env.PORT || 8081),
    verbose = (process.env.VERBOSE || null),
    http = require('http'),
    ecstatic = require('ecstatic'),
    server = http.createServer(
      ecstatic({ root: __dirname })
    ),
    WebSocketServer = require('../'),
    app = require('./server.js').Server.worker(verbose),
    wss = new WebSocketServer(
      server,
      app.ports.inputPort,
      app.ports.outputPort
    );

server.listen(port);

console.log(`Listening on :${port}`);
