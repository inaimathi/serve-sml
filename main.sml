structure Serv = SERVER (structure Buf = DefaultBuffer; structure Par = BasicHTTP);

(* curl -d "a=foo&b=bar" localhost:8787/paramtest *)

fun httpRes responseType extraHeaders body action = 
    ({
	httpVersion = "HTTP/1.1", responseType = responseType, 
	headers = ("Content-Length", Int.toString (String.size body))::extraHeaders,
	body = body
    }, action)

fun route f (request : Serv.Request) socket =
    let val path = String.tokens (curry (op =) #"/") (Serv.resource request)
	val (res, act) = f (Serv.method request) path (Serv.param request)
    in
	Serv.sendRes socket res;
	act
    end

fun simpleRes resCode body =
    httpRes resCode [] body Serv.CLOSE;

fun ok body = httpRes "200 Ok" [] body Serv.CLOSE;
fun err400 body = httpRes "404 Not Found" [] body Serv.CLOSE;
fun err404 body = httpRes "404 Not Found" [] body Serv.CLOSE;

fun hello "GET" ["hello", name] _ = 
    ok ("Hello there, " ^ name ^ "! I'm a server!")
  | hello _ ("rest"::rest) _ =
    ok ("You asked for: '" ^ (String.concatWith " :: " rest) ^ "' ...")
  | hello _ ["paramtest"] ps =
    let in
	case (ps "a", ps "b") of
	    (SOME a, SOME b) =>
	    ok ("You sent me A: " ^ a ^ " and B: " ^ b ^ "...")
	  | _ => err400 "Need both 'a' and 'b' parameters for this one."
    end
  | hello _ _ _ = 
    err404 "Sorry; I don't know how to do that";

fun getPort (port::_) = 
    let fun p (SOME n) = n
	  | p NONE = 8181
    in 
	p (Int.fromString port)
    end
  | getPort _ = 8181;

Serv.serve (getPort (CommandLine.arguments ())) (route hello) ;
