(* ***** Response generation *)
fun httpOK extraHeaders body = 
    {
      httpVersion = "HTTP/1.1", responseType = "200 OK", 
      headers = ("Content-Length", Int.toString (String.size body))::extraHeaders,
      body = body
    };

fun sendResponse sock {httpVersion, responseType, headers, body} =
    let val toSlc = Word8VectorSlice.full o Byte.stringToBytes
	fun vec slc = Socket.sendVec (sock, slc)
	val str = vec o toSlc
	val crlf = toSlc "\r\n"
	fun ln lst = (each str lst; vec crlf)
    in 
	ln [httpVersion, " ", responseType];
	each (fn (k, v) => ln [k, ": ", v]) headers;
	vec crlf;
	str body
    end;

(* ***** Dummy server *)
fun helloServer (request : DefaultHTTPParser.Request) socket =
    let val body = "You asked for '" ^ (#resource request) ^ "' ..."
    in
	print "Sending...\n";
	(sendResponse socket (httpOK [] body));
	CLOSE
    end;

fun getPort (port::_) = 
    let fun p (SOME n) = n
	  | p NONE = 8181
    in 
	p (Int.fromString port)
    end
  | getPort _ = 8181;

serve (getPort (CommandLine.arguments ())) helloServer ;
