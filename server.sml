type Request = { method : string, resource : string, httpVersion : string,
		 headers : (string * string) list, 
		 parameters : (string * string) list }
type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }

datatype SockAction = CLOSE | LEAVE_OPEN

(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun curry f = fn a => fn b => f(a,b)

fun each f [] = ()
  | each f (e::es) = (f e; each f es)

(* ***** Response generation *)
fun httpOK extraHeaders body = 
    {
      httpVersion = "HTTP/1.1", responseType = "200 OK", 
      headers = ("Content-Length", Int.toString (String.size body))::extraHeaders,
      body = body
    }

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
    end

(* ***** Dummy server *)
fun helloServer port (request : Request) socket =
    let val body = "You asked for '" ^ (#resource request) ^ "' on port " ^ (Int.toString port) ^ "..."
    in
	print "Sending...\n";
	(sendResponse socket (httpOK [] body));
	CLOSE
    end

(* ***** Toy server *)
fun processClients _ _ [] = []
  | processClients _ [] rest = rest
  | processClients f (d::ds) ((c, buffer)::cs) = 
    if d = (Socket.sockDesc c)
    then case DefaultBuffer.readInto buffer c of
	     Complete => let in
			     DefaultBuffer.printBuffer buffer;
			     if CLOSE = f 8181 (DefaultParser.parse (DefaultBuffer.toSlice buffer)) c
			     then (Socket.close c; processClients f ds cs)
			     else (c, DefaultBuffer.new 2000) :: (processClients f ds cs)
			 end
	   | Incomplete => (c, buffer) :: (processClients f (d::ds) cs)
	   | Errored    => (Socket.close c; (processClients f ds cs))
    else (c, buffer) :: (processClients f (d::ds) cs)

fun processServers _ [] = []
  | processServers [] _ = []
  | processServers (d::ds) (s::ss) = 
    if d = (Socket.sockDesc s)
    then let val c = fst (Socket.accept s)
	     val buf = DefaultBuffer.new 2000
	 in 
	     (c, buf) :: (processServers ds ss)
	 end
    else processServers (d::ds) ss

fun selecting server clients timeout =
    let val { rds, exs, wrs } = Socket.select {
		rds = (Socket.sockDesc server) :: (map Socket.sockDesc clients),
		wrs = [], exs = [], timeout = timeout
	    }
    in
	rds
    end
    handle x => (Socket.close server; raise x)

fun acceptLoop serv clients serverFn =
    let val ready = selecting serv (map fst clients) NONE
	val newCs = processServers ready [serv]
	val next = processClients serverFn ready clients
    in
	acceptLoop serv (newCs @ next) serverFn
    end
    handle x => (Socket.close serv; raise x)

fun serve port serverFn =
    let val s = INetSock.TCP.socket()
    in 
	Socket.Ctl.setREUSEADDR (s, true);
	Socket.bind(s, INetSock.any port);
	Socket.listen(s, 5);
	print "Entering accept loop...\n";
	acceptLoop s [] serverFn
    end
