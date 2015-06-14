structure Server = SERVER (structure Buf = DefaultBuffer; structure Par = DefaultHTTPParser);
structure Route = BASICROUTER(structure Serv = Server);

fun simpleRes resCode body =
    Route.httpRes resCode [] body CLOSE;

fun hello "GET" ["hello", name] _ = 
    simpleRes "200 Ok" ("Hello there, " ^ name ^ "! I'm a server!")
  | hello _ _ _ = 
    simpleRes "404 Not Found" "Sorry; I don't know how to do that"

fun getPort (port::_) = 
    let fun p (SOME n) = n
	  | p NONE = 8181
    in 
	p (Int.fromString port)
    end
  | getPort _ = 8181;

Server.serve (getPort (CommandLine.arguments ())) (Route.route hello) ;
