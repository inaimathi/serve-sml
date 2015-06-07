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
