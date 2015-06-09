datatype Method = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH
type Request = { method : Method, resource : string, httpVersion : string,
		 headers : (string * string) list, 
		 parameters : (string * string) list }
type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }

(* ***** Dummy Parser *)
fun httpParse (arr : Word8Array.array) =
    { method = GET, resource = "/test", httpVersion = "1.1"
      , headers = [("Content-type", "mumble"), ("Content-length", "boogle")]
      , parameters = [("foo", "1"), ("bar", "2"), ("baz", "3")]
    } : Request

(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun consOpt NONE lst = lst
  | consOpt (SOME e) lst = e::lst

fun curry f = fn a => fn b => f(a,b)

(* ***** Toy server *)
fun sendHello sock = 
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 14\r\n\r\nHello from ML!\r\n\r\n"
	val slc = Word8VectorSlice.full (Byte.stringToBytes res)
	fun got NONE = print "Received nothing..."
	  | got (SOME b) = DefaultBuffer.printBuffer b
	val buf = DefaultBuffer.new 2000
	val req = DefaultBuffer.readInto buf sock
    in 
	if req
	then printBuf buf
	else print "Received nothing...";
	print "\n";
	print "Sending...\n";
	Socket.sendVec (sock, slc);
	Socket.close sock;
	NONE		     
    end
    handle x => (Socket.close sock; raise x)

fun processReady f restFn (d::ds) (s::ss) acc = 
    if d = (Socket.sockDesc s)
    then processReady f restFn ds ss (f s acc)
    else processReady f restFn (d::ds) ss acc
  | processReady f restFn _ rest acc = restFn (rest, acc)

fun processClients descs socks = 
    let fun f s acc = consOpt (sendHello s) acc
    in 
	processReady f (op @) descs socks []
    end

fun processServers descs socks =
    let fun f s acc = (fst (Socket.accept s))::acc
    in 
	processReady f snd descs socks []
    end 

fun selecting server clients timeout =
    let val { rds, exs, wrs } = Socket.select {
		rds = (Socket.sockDesc server) :: (map Socket.sockDesc clients),
		wrs = [], exs = [], timeout = timeout
	    }
    in
	rds
    end
    handle x => (Socket.close server; raise x)

fun acceptLoop serv clients =
    let val ready = selecting serv clients NONE
	val newCs = processServers ready [serv]
	val next = processClients ready clients
    in
	acceptLoop serv (newCs @ next)
    end
    handle x => (Socket.close serv; raise x)

fun serve port =
    let val s = INetSock.TCP.socket()
    in 
	Socket.Ctl.setREUSEADDR (s, true);
	Socket.bind(s, INetSock.any port);
	Socket.listen(s, 5);
	print "Entering accept loop...\n";
	acceptLoop s []
    end
