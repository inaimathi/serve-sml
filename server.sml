datatype Method = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH
type Request = { method : Method, resource : string, httpVersion : string,
		 headers : (string * string) list, 
		 parameters : (string * string) list }
type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }

datatype SockAction = CLOSE | LEAVE_OPEN

(* ***** Dummy Parser *)
fun httpParse (arr : Word8Array.array) =
    { method = GET, resource = "/test", httpVersion = "1.1"
      , headers = [("Content-type", "mumble"), ("Content-length", "boogle")]
      , parameters = [("foo", "1"), ("bar", "2"), ("baz", "3")]
    } : Request

(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun curry f = fn a => fn b => f(a,b)

(* ***** Toy server *)
fun helloServer port request socket =
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 14\r\n\r\nHello from ML!\r\n\r\n"
	val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in
	print "Sending...\n";
	Socket.sendVec (socket, slc);
	CLOSE
    end

fun processClients _ _ [] = []
  | processClients _ [] rest = rest
  | processClients f (d::ds) ((c, buffer)::cs) = 
    if d = (Socket.sockDesc c)
    then case DefaultBuffer.readInto buffer c of
	     Complete => if CLOSE = f 8181 (httpParse (#buf buffer)) c
			 then (Socket.close c; processClients f ds cs)
			 else (c, DefaultBuffer.new 2000) :: (processClients f ds cs)
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
