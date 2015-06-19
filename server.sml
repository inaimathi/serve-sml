signature HTTPSERVER =
sig
    datatype SockAction = CLOSE | LEAVE_OPEN | KEEP_LISTENING
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
  datatype SockAction = CLOSE | LEAVE_OPEN | KEEP_LISTENING
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
      datatype ReqState = INITIAL_READ | BODY_READ of Request

      fun completeRequest c req cb=
	  case cb req of
	      CLOSE => NONE
	    | _ => SOME (c, Buf.new (), INITIAL_READ, cb)

      fun processRequest (c, slc, INITIAL_READ, cb) =
	  let val req = Par.parseReq slc
	      fun complete () = completeRequest c req cb
	  in 
	      case (Par.method req, Par.header req "Content-Type", Par.header req "Content-Length") of
		  ("POST", SOME "application/x-www-form-urlencoded", SOME ct) =>
		  (case Int.fromString ct of
		       NONE => complete ()
		     | SOME n => SOME (c, Buf.newStatic n, BODY_READ req, cb))
		| _ => complete ()
	  end
	| processRequest (c, slc, BODY_READ saved, cb) = 
	  let val params = Par.parseParams slc 
	  in 
	      completeRequest c (Par.addParams saved params) cb
	  end

      fun processClients descriptors sockTuples =
	  let fun recur _ [] = []
		| recur [] rest = rest
		| recur (d::ds) ((c,buffer,state,cb)::cs) = 
		  if Socket.sameDesc (d, Socket.sockDesc c)
		  then (case Buf.readInto buffer c of
			    Complete => (case processRequest (c, Buf.toSlice buffer, state, cb) of
					     SOME tup => tup :: (recur ds cs)
					   | NONE => (Socket.close c; recur ds cs) )
			  | Incomplete => (c, buffer, state, cb) :: (recur ds cs)
			  | Errored => (Socket.close c; recur ds cs) (* Should send 400 here *)
			  | Dead => (Socket.close c; recur ds cs))
		       handle Fail _ => (Socket.close c; recur ds cs)
		  else (c, buffer, state, cb) :: (recur (d::ds) cs)
	  in 
	      recur descriptors sockTuples
	  end

      fun processServers f descs socks =
	  let fun recur ds [] newClients = (ds, newClients)
		| recur [] _ newClients = ([], newClients)
		| recur (d::ds) (s::ss) newClients = 
		  if Socket.sameDesc (d, Socket.sockDesc s)
		  then let val c = fst (Socket.accept s)
			   val buf = Buf.new ()
			   fun cb req = f req c
		       in 
			   recur ds ss ((c, buf, INITIAL_READ, cb)::newClients)
		       end
		  else recur (d::ds) ss newClients
	  in 
	      recur descs socks []
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

      fun acceptLoop serv clients serverFn =
	  let val ready = selecting serv (map (fn (a, _, _, _) => a) clients) NONE
	      val (stillReady, newCs) = processServers serverFn ready [serv]
	      val next = processClients stillReady clients
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
