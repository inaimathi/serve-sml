datatype Method = GET | HEAD | POST | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH
type Request = { method : Method, resource : string, httpVersion : string,
		 headers : (string * string) list, 
		 parameters : (string * string) list }
type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }

(* ***** Buffering machinery *)
type Buffer = { fill : int ref, buf : Word8Array.array }

fun test_isTerminated () =
    let val arr = map (Word8.fromInt o Char.ord) (String.explode "Testing \r\n\r\n ")
	val b = { fill = ref 12, buf = Word8Array.fromList arr}
    in 
	(Word8Array.sub (#buf b, !(#fill b)), isTerminated b)
    end

fun isTerminated {fill, buf} = 
    let fun chk c i = (Word8.fromInt (Char.ord c)) = (Word8Array.sub (buf, !fill - i))
    in 
	(!fill >= 4) andalso (chk #"\r" 4) andalso (chk #"\n" 3) andalso (chk #"\r" 2) andalso (chk #"\n" 1)
    end

fun readInto (buffer : Buffer) sock = 
    let val i = Socket.recvArrNB (sock, Word8ArraySlice.slice (#buf buffer, !(#fill buffer), SOME 1))
	fun bump NONE = ()
	  | bump (SOME n) = (#fill buffer := (!(#fill buffer) + n); ())
    in 
	bump i;
	if isTerminated buffer
	then SOME buffer
	else if i = NONE
	then NONE
	else readInto buffer sock
    end

(* ***** Debugging utility *)
fun print8 b = 
    let val c = (Char.chr (Word8.toInt b))
    in 
	print (Char.toString c);
	if c = #"\n" then print "\n   " else print "";
	()
    end 

fun printBuf {fill, buf} =
    let val slc = Word8ArraySlice.slice (buf, 0, SOME (!fill))
    in
	print "\n   ";
	Word8ArraySlice.app print8 slc;
	()
    end

(* fun printReq vec = *)
(*     (print "\n   "; *)
(*      Word8Vector.map print8 vec; *)
(*      ()) *)

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
	  | got (SOME b) = printBuf b
	val buf = { fill= ref 0, buf= Word8Array.array (2000, Word8.fromInt 0) }
	val req = readInto buf sock
    in 
	got req;
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
