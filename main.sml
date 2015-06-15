structure Server = SERVER (structure Buf = DefaultBuffer; structure Par = DefaultHTTPParser);
structure Route = BASICROUTER(structure Serv = Server);

fun simpleRes resCode body =
    Route.httpRes resCode [] body CLOSE;

fun ok body = Route.httpRes "200 Ok" [] body CLOSE;
fun err400 body = Route.httpRes "404 Not Found" [] body CLOSE;
fun err404 body = Route.httpRes "404 Not Found" [] body CLOSE;

fun hello "GET" ["hello", name] _ = 
    ok ("Hello there, " ^ name ^ "! I'm a server!")
  | hello _ ("rest"::rest) _ =
    ok ("You asked for: " ^ (String.concatWith "/" rest) ^ "' ...")
  | hello _ ["paramtest"] param =
    let in
	case (param "a", param "b") of
	    (SOME a, SOME b) =>
	    ok ("You sent me A: " ^ a ^ " and B: " ^ b ^ "...")
	  | _ => err400 "Need both 'a' and 'b' parameters for this one."
    end
  | hello _ _ _ = 
    err404 "Sorry; I don't know how to do that"

fun getPort (port::_) = 
    let fun p (SOME n) = n
	  | p NONE = 8181
    in 
	p (Int.fromString port)
    end
  | getPort _ = 8181;

Server.serve (getPort (CommandLine.arguments ())) (Route.route hello) ;
