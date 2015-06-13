# SML Server
###### An attempt at a web server in Standard ML

## Usage

### With `SML/NJ`

- Start `sml` in the project directory
- Enter `use "buffer.sml" ; use "parser.sml" ; use "server.sml" ; use "main.sml" ;`
- Navigate to `http://localhost:8181`

### With `mlton`

- `cd` into the project directory
- Enter `mlton server.mlb`
- Run `./server 8181`
- Navigate to `http://localhost:8181`


## Status

- Don't use it yet, it's nowhere near done
- Now have minimally working "hello world" server
- Still need to think about how I'm going to want to define handlers

### ToDo

##### Items

- Functorify the server core
- Make buffers naively growable (again, no optimization; give people the hooks to make better buffers/parsers and just lay the groundwork)

##### Musings

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

### Test Data

Get request:

    "GET / HTTP/1.1\r\nHost: localhost:8184\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nCookie: __utma=111872281.1074254706.1427666251.1427666251.1427666251.1; __utmz=111872281.1427666251.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); C6gbrqqYAK3a1rKin6QaTAZDD5Oe0xnRat0RKe06ntufdcKUN12VtUXc8rfLrgw4\r\nConnection: keep-alive\r\n\r\n"
