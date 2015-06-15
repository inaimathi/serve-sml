signature HTTP =
sig
    type Response
    type ResponseType
    val response : ResponseType -> (string * string) list -> string -> Response
    val parseRes : Word8ArraySlice.slice -> Response
    val resHeader : Response -> string -> string option
    val body : Response -> string
    val sendRes : ('a,Socket.active Socket.stream) Socket.sock -> Response -> unit

    type Request
    val request : string -> string -> (string * string) list -> (string * string) list -> Request
    val parseReq : Word8ArraySlice.slice -> Request
    val sendReq : ('a,Socket.active Socket.stream) Socket.sock -> Request -> unit
    val header : Request -> string -> string option
    val param : Request -> string -> string option
    val mapParams : Request -> (string * string -> 'a) -> 'a list
    val addParam : Request -> string -> string -> Request
    val method : Request -> string
    val resource : Request -> string
end

structure BasicHTTP : HTTP =
struct
  type ResponseType = string

  type Response = { httpVersion : string, responseType : ResponseType,
		    headers : (string * string) list, 
		    body : string }

  type Request = { method : string, resource : string, httpVersion : string,
		   headers : (string * string) list, 
		   parameters : (string * string) list }
  type Headers = (string * string) list
  type Parameters = (string * string) list

  local
      fun matches str arr =
	  ((String.size str) = (Word8ArraySlice.length arr)) 
	  andalso let val chr = Word8.fromInt o Char.ord
		      fun recur (~1) = true
			| recur i = ((chr (String.sub (str, i))) = (Word8ArraySlice.sub (arr, i)))
				    andalso recur (i - 1)
		  in 
		      recur ((String.size str) - 1)
		  end 

      fun strToSlice str = (Word8ArraySlice.full (Word8Array.fromList (map (Word8.fromInt o Char.ord) (String.explode str))))
      fun sliceToStr slice = 
	  let val len = Word8ArraySlice.length slice
	      fun f i = Char.chr (Word8.toInt (Word8ArraySlice.sub (slice, i)))
	  in 
	      String.implode (List.tabulate (len, f))
	  end

      fun tokens sep arr =
	  let val lst = map (Word8.fromInt o Char.ord) (String.explode sep)
	      val sepLen = String.size sep
	      val len = Word8ArraySlice.length arr
	      fun collect mark i sepLen acc = 
		  if i > (mark + sepLen)
		  then (Word8ArraySlice.subslice (arr, mark, SOME ((i-mark)-sepLen)))::acc
		  else acc
	      fun recur mark i [] acc = recur i i lst (collect mark i sepLen acc)
		| recur mark i (b::bs) acc = if i = len
					     then List.rev (collect mark i 0 acc)
					     else if b = (Word8ArraySlice.sub (arr, i))
					     then recur mark (i+1) bs acc
					     else recur mark (i+1) lst acc
	  in 
	      recur 0 0 lst []
	  end

      fun parseParams slc =
	  let val pairs = tokens "&" slc
  	      fun toPair [k, v] = (sliceToStr k, sliceToStr v)
  		| toPair _ = raise Fail "Invalid parameter"
	  in
  	      map (toPair o tokens "=") pairs
	  end

      fun lookup _ [] = NONE
	| lookup key ((k : string, v :string)::rest) = 
	  if key = k
	  then SOME v
	  else lookup key rest
		      
      val crlf = Word8VectorSlice.full (Byte.stringToBytes "\r\n")

      fun sendString sock str =
	  Socket.sendVec (sock, Word8VectorSlice.full (Byte.stringToBytes str))

      fun each f [] = ()
	| each f (e::es) = (f e; each f es)

      fun sendLine sock ln = 
	  let val snd = sendString sock
	  in
	      each snd ln;
	      Socket.sendVec (sock, crlf)
	  end
  in 

  fun response tp headers body = 
      {
	httpVersion="HTTP/1.1", responseType=tp,
	headers=headers, body=body
      }
  fun resHeader (res : Response) key = lookup key (#headers res)
  fun body (res : Response) = #body res

  fun parseRes slc = 
      let val (req, headers) = case tokens "\r\n" slc of
				   (ln::lns) => (ln, lns)
				 | _ => raise Fail "Invalid request"
	  val (version, rType) = case tokens " " req of
				     (v::rest) => (v, rest)
				   | _ => raise Fail "Invalid request line"
	  fun toHdr [k, v] = (sliceToStr k, sliceToStr v)
	    | toHdr _ = raise Fail "Invalid header"
      in 
	  { 
	    httpVersion= sliceToStr version, responseType= String.concatWith " " (map sliceToStr rType),
	    headers = map (fn h => toHdr (tokens ": " h)) headers,
	    body=""
	  }
      end

  fun sendRes sock {httpVersion, responseType, headers, body} =
      let fun ln lst = sendLine sock lst
      in 
	  ln [httpVersion, " ", responseType];
	  each (fn (k, v) => ln [k, ": ", v]) headers;
	  ln [];
	  ln [body];
	  ()
      end

  fun request method res headers params = 
      { 
	httpVersion="HTTP/1.1", method=method, resource=res, 
	headers=headers, parameters=params
      }

  fun header (req : Request) name = lookup name (#headers req)
  fun param (req : Request) name = lookup name (#parameters req)

  fun mapParams (req : Request) f = map f (#parameters req)
  fun addParam {method, resource, httpVersion, headers, parameters } k v =
      { method=method, resource=resource, httpVersion=httpVersion,
	headers=headers, parameters= (k, v)::parameters}

  fun method (req : Request) = #method req
  fun resource (req : Request) = #resource req

  fun parseReq slc =
      let val (req, rest) = case tokens "\r\n" slc of
				(r :: rs) => (r, rs)
			      | _ => raise Fail "Invalid request"
	  fun toHdr [k, v] = (sliceToStr k, sliceToStr v)
	    | toHdr _ = raise Fail "Invalid header"
	  fun toReq [m, uri, ver] hdrs = 
	      let val (resource, args) = case tokens "?" uri of
		  			     [rawUri, ps] => (rawUri, parseParams ps)
		  			   | [rawUri] => (rawUri, [])
					   | _ => raise Fail "Invalid resource specifier"
	      in { method=sliceToStr m, resource=sliceToStr resource, httpVersion=sliceToStr ver,
		   headers= map (fn h => toHdr (tokens ": " h)) hdrs,
		   parameters=args }
	      end
	    | toReq _ _ = raise Fail "Invalid request line"
      in 
	  ((toReq (tokens " " req) rest) : Request)
      end					      

  fun sendReq sock {httpVersion, method, resource, headers, parameters} =
      let val ln  = sendLine sock
	  val str = sendString sock
	  val paramStr = String.concatWith "&" (map (fn (k, v) => k ^ "=" ^ v) parameters)
	  val (uri, hs, bdy) = case method of
				   "POST" => (resource,
					     ("Content-Length", Int.toString (String.size paramStr))
					     ::("Content-Type", "application/x-www-form-urlencoded")
					     ::headers,
					     paramStr)
				 | _ => (resource ^ "?" ^ paramStr, headers, "")
      in
	  ln [method, " ", uri, " HTTP/1.1"];
	  each (fn (k, v) => ln [k, ": ", v]) hs;
	  ln [];
	  if bdy = ""
	  then ()
	  else (str bdy; ())
      end
	  
  end
end
