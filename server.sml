(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun a_ (a, _, _) = a
fun b_ (_, b, _) = b
fun c_ (_, _, c) = c

fun curry f = fn a => fn b => f(a,b)

fun each f [] = ()
  | each f (e::es) = (f e; each f es)

datatype SockAction = CLOSE | LEAVE_OPEN | KEEP_LISTENING

(* ***** Server core *)
signature TCPSERVER =
sig
    type Request
    val serve : int -> (Request -> (INetSock.inet,Socket.active Socket.stream) Socket.sock -> SockAction) -> 'u

    val header : Request -> string -> string option
    val param : Request -> string -> string option
    val mapParams : Request -> (string * string -> 'a) -> 'a list
    val addParam : Request -> string -> string -> Request
    val method : Request -> string
    val resource : Request -> string
end

functor SERVER (structure Buf : BUFFER; structure Par : PARSER) : TCPSERVER =
struct
  type Request = Par.Request
  val header = Par.header
  val param = Par.param
  val mapParams = Par.mapParams
  val addParam = Par.addParam
  val method = Par.method
  val resource = Par.resource
  local
      fun processClients f descriptors sockBufferPairs =
	  let fun recur _ [] = []
		| recur [] rest = rest
		| recur (d::ds) ((c,buffer)::cs) = 
		  if Socket.sameDesc (d, Socket.sockDesc c)
		  then case Buf.readInto buffer c of
			   Complete => if CLOSE = f (Par.parse (Buf.toSlice buffer)) c
				       then (Socket.close c; recur ds cs)
				       else (c, Buf.new 1000) :: (recur ds cs)
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
		   val buf = Buf.new 1000
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
  in 
  
  fun serve port serverFn =
      let val s = INetSock.TCP.socket()
      in 
	  Socket.Ctl.setREUSEADDR (s, true);
	  Socket.bind(s, INetSock.any port);
	  Socket.listen(s, 5);
	  print ("Listening on port " ^ (Int.toString port) ^ "...\n");
	  acceptLoop s [] serverFn
      end

  end
end
