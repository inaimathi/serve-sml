type Response = { httpVersion : string, responseType : string,
		  headers : (string * string) list, 
		  body : string }

datatype SockAction = CLOSE | LEAVE_OPEN

(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun a_ (a, _, _) = a
fun b_ (_, b, _) = b
fun c_ (_, _, c) = c

fun curry f = fn a => fn b => f(a,b)

fun each f [] = ()
  | each f (e::es) = (f e; each f es)

(* ***** Server core *)
fun processClients f descriptors sockBufferPairs =
    let fun recur _ [] = []
	  | recur [] rest = rest
	  | recur (d::ds) ((c,buffer)::cs) = 
	    if Socket.sameDesc (d, Socket.sockDesc c)
	    then case DefaultBuffer.readInto buffer c of
		     Complete => if CLOSE = f (DefaultHTTPParser.parse (DefaultBuffer.toSlice buffer)) c
				 then (Socket.close c; recur ds cs)
				 else (c, DefaultBuffer.new 2000) :: (recur ds cs)
		   | Incomplete => (c, buffer) :: (recur ds cs)
		   | Errored => (Socket.close c; recur ds cs)
		 handle Fail _ => (Socket.close c; recur ds cs)
	    else (c, buffer) :: (recur (d::ds) cs)
    in 
	recur descriptors sockBufferPairs
    end

fun processServers _ [] = []
  | processServers [] _ = []
  | processServers (d::ds) (s::ss) = 
    if Socket.sameDesc (d, Socket.sockDesc s)
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
	print ("Listening on port " ^ (Int.toString port) ^ "...\n");
	acceptLoop s [] serverFn
    end
