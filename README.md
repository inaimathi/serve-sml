# SML Server
###### An attempt at a web server in Standard ML

- Now have minimally working multi-client server using `Socket.select`
- Need to think a bit about (these points will probably be related)
	1. Along what dimensions we'd like to customize this server
	2. How we'll write handlers
	3. How to expose this functionality via structures


### ToDo

Usual structure.

        buffer -> parse -> handleRequest

1. Buffer collects bytes, emits `(socket, byte list)`
2. Parse takes `byte list`, emits a `request` structure
3. `handleRequest` takes a `socket -> request -> ()`, and writes a response to the socket

The user of this library would need to hand over a `(uri, (string string map) -> byte vector) list` (the HTTP handlers table). We might want to customize

- the HTTP parser
- the routing
- we might want to add SSE/session management (we can do that by transforming the incoming handler table)

SO.

We'll want to be able to customize

1. The buffering strategy
2. The parser
3. The number of listening ports
4. The handling function

- The first two can be module-level
	- we'll define a `DefaultBuffer` and `DefaultParser` struct with a minimal interface, pass them into the `HTTPServer` struct so that they can be readily replaced.
- The third and fourth are changes to the input of `HTTPServer.serve`.
	- Instead of `(int -> ())`, it'll be `([int] -> (int -> Request -> socket -> socket option)) -> ())`. That is, a list of ports to listen on, and a function that takes a port, a `Request` and the client socket, and does something.
	- Should we restrict what it can do with a client socket? We'd do this by passing `write`/`read`/`close`/`register` callbacks instead of the socket itself. Not sure. I kind of feel like I should be presenting as general an interface as possible at this stage. We can always define a specializing function in a higher-level framework, but we won't necessarily be able to pry the socket back out if we go the other way.
- We also might want to be able to change out the representation of request parameters. This would probably be another module-level input. However...
	- Not sure I'd want to allow changing out `Request` representation piece-wise, or just allow a full `Request` representation to be passed in...
