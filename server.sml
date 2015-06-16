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
signature HTTPSERVER =
sig
    type Request
    val serve : int -> (Request -> (INetSock.inet,Socket.active Socket.stream) Socket.sock -> SockAction) -> 'u

    val header : Request -> string -> string option
    val param : Request -> string -> string option
    val mapParams : Request -> (string * string -> 'a) -> 'a list
    val addParam : Request -> string -> string -> Request
    val method : Request -> string
    val resource : Request -> string

    type Response
    val resHeader : Response -> string -> string option
    val body : Response -> string

    val sendRes : ('a,Socket.active Socket.stream) Socket.sock -> Response -> unit
    val sendReq : ('a,Socket.active Socket.stream) Socket.sock -> Request -> unit
end

functor SERVER (structure Buf : BUFFER; structure Par : HTTP) : HTTPSERVER =
struct
  type Request = Par.Request
  val header = Par.header
  val param = Par.param
  val mapParams = Par.mapParams
  val addParam = Par.addParam
  val method = Par.method
  val resource = Par.resource

  type Response = Par.Response
  val resHeader = Par.resHeader
  val body = Par.body

  val sendRes = Par.sendRes
  val sendReq = Par.sendReq

  local
      fun processClients descriptors sockTrips =
	  let fun recur _ [] = []
		| recur [] rest = rest
		| recur (d::ds) ((c,buffer,cb)::cs) = 
		  if Socket.sameDesc (d, Socket.sockDesc c)
		  then case Buf.readInto buffer c of
			   Complete => let val req = (Par.parseReq (Buf.toSlice buffer))
				       in 
					   (* TODO - Figure out how to buffer more, only the first time here.
					             That way we can automatically read POST body in certain
					             circumstances *)
					   if CLOSE = cb req
					   then (Socket.close c; recur ds cs)
					   else (c, Buf.new 1000, cb) :: (recur ds cs)
				       end
			 | Incomplete => (c, buffer, cb) :: (recur ds cs)
			 | Errored => (Socket.close c; recur ds cs)
				      handle Fail _ => (Socket.close c; recur ds cs)
		  else (c, buffer, cb) :: (recur (d::ds) cs)
	  in 
	      recur descriptors sockTrips
	  end

      fun processServers _ _ [] = []
	| processServers _ [] _ = []
	| processServers f (d::ds) (s::ss) = 
	  if Socket.sameDesc (d, Socket.sockDesc s)
	  then let val c = fst (Socket.accept s)
		   val buf = Buf.new 1000
		   fun cb req = f req c
	       in 
		   (c, buf, cb) :: (processServers f ds ss)
	       end
	  else processServers f (d::ds) ss

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
	  let val ready = selecting serv (map a_ clients) NONE
	      val newCs = processServers serverFn ready [serv]
	      val next = processClients ready clients
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
