type Request = { method : string, resource : string, httpVersion : string,
		 headers : (string * string) list, 
		 parameters : (string * string) list }

signature HTTPPARSER =
  sig
      (* type Request *)
      val parse : Word8ArraySlice.slice -> Request
      val parseParams : Word8ArraySlice.slice -> (string * string) list
      val tokens : string -> Word8ArraySlice.slice -> Word8ArraySlice.slice list
      val sliceToStr : Word8ArraySlice.slice -> string
      val strToSlice : string -> Word8ArraySlice.slice
      (* val header : Request -> string -> string *)
      (* val setHeader : Request -> string -> string -> Request *)
      (* val param : Request -> string -> string *)
      (* val addParam : Request -> string -> string -> Request *)
      (* val method : Request -> string *)
      (* val resource : Request -> string *)
  end

structure DefaultParser : HTTPPARSER =
  struct
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

  in
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

  fun parse slc =
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
  end
  end
