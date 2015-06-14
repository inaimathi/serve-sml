signature ROUTER =
sig
    type Response
    type Method
    type Path
    type Request
    val send : ('a,Socket.active Socket.stream) Socket.sock -> Response -> unit
    val httpRes : string -> (string * string) list -> string -> SockAction -> (Response * SockAction)
    val route : (Method -> Path -> (string -> string option) -> (Response * SockAction)) -> Request -> ('a,Socket.active Socket.stream) Socket.sock -> SockAction
end

functor BASICROUTER (structure Serv : TCPSERVER) : ROUTER =
struct

type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }
type Method = string
type Path = string list
type Request = Serv.Request

fun send sock {httpVersion, responseType, headers, body} =
    let val toSlc = Word8VectorSlice.full o Byte.stringToBytes
	fun vec slc = Socket.sendVec (sock, slc)
	val str = vec o toSlc
	val crlf = toSlc "\r\n"
	fun ln lst = (each str lst; vec crlf)
    in 
	ln [httpVersion, " ", responseType];
	each (fn (k, v) => ln [k, ": ", v]) headers;
	vec crlf;
	str body;
	()
    end

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
	send socket res;
	act
    end
end
