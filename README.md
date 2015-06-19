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

- Do a cleanup round
- Start URI decoding parameters
- Think about how to do `application/json` and `multipart/form-data` parsing for POST bodies
- Start thinking about the general handler structure
	- routing needs to be handled (with path variables)
	- defining handlers needs to be handled (hehe). Including
		- setting headers,
		- setting response type
		- sending a body easily (take a string, compute length from it, attach the header if not otherwise provided and send out the response)
- Deal elegantly with making requests from handler bodies

##### Relevant Musings

- The basic server is now `structure`/`functor`-ified
	- Because of the way it's done, this doesn't necessarily have to represent an HTTP server. This particular implementation happens to, but it seems like this could let you build a server for any stream-socket protocol. Not actually sure about "any", but it seems like it can do more than just HTTP.
- Response-wise, we want to support (in descending order of priority)
	- Standard HTTP responses
	- SSE interaction (keep the socket off to the side, complete with channels, periodically send things to them)
	- Websockets (keep sockets in the main listen loop, but do different things with them. Not sure how this is actually going to work, given that the main listener loop will always expect `\r\n` delimiters, it seems this would limit what kind of protocol you could set up. Do we need an arbitrary async read from handlers?)
	- Static file serving
	- Static directory-tree serving
- It'd be nice if we had a baked-in async client that worked with the same main event-loop to let you make low-friction requests between servers
- What we ultimately want to pass into `serve` is s function that does `(Request -> socket -> ServAction)`. Providing a simple and flexible way of constructing that function is going to be most of the rest of the work.
- We'll want an easy way of associating routes (including path variables) with functions of `(Parameter list -> Response)`. Maybe even push that to `string` at the output (just the response body). Ditto for SSE sends to a particular channel. Arbitrary socket sends (`Websockets`) will be more involved and fine-grained, because they'll by definition depend on the application-specific message format being used.
- Do we want routing to be a separate component entirely? Or just make the responder simple enough that the entire thing can be replaced? Every point of customization adds a little bit of extra complexity in terms of implementation, but I'm convinced we can still use plain `fun`s and `struct`s to present a simple interface.
- Do we need to be able to control whether a socket closes on a per-handler level? It seems like we might just need multiple handler types. Specifically, if we had something like `ChannelHandlers` that subscribed new sockets, `WSHandlers` that dealt with WebSockets, we could let everything else close (possibly based on incoming headers)

### Test Data

Get request:

    "GET / HTTP/1.1\r\nHost: localhost:8184\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nCookie: __utma=111872281.1074254706.1427666251.1427666251.1427666251.1; __utmz=111872281.1427666251.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); C6gbrqqYAK3a1rKin6QaTAZDD5Oe0xnRat0RKe06ntufdcKUN12VtUXc8rfLrgw4\r\nConnection: keep-alive\r\n\r\n"
