signature HTTPParser =
  sig
      val parse : Word8ArraySlice.slice -> Request
  end

structure DefaultParser : HTTPParser =
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

      fun sliceToStr slice = 
	  let val len = Word8ArraySlice.length slice
	      fun f i = Char.chr (Word8.toInt (Word8ArraySlice.sub (slice, i)))
	  in 
	      String.implode (List.tabulate (len, f))
	  end
	      
      fun tokens sep arr =
	  let val lst = map (Word8.fromInt o Char.ord) (String.explode sep)
	      val sepLen = String.size sep
	      fun ct mark i = SOME ((i - mark) - sepLen)
	      val len = Word8ArraySlice.length arr
	      fun collect mark i sepLen acc = 
		  if i > (mark + sepLen)
		  then let val sub = Word8ArraySlice.subslice (arr, mark, SOME ((i-mark)-sepLen))
		       in 
			   if matches sep sub
			   then acc
			   else sub::acc
		       end
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
  in 
  fun parse slc =
      let val (req :: rest) = tokens "\r\n" slc
	  fun toHdr [k, v] = (sliceToStr k, sliceToStr v)
	    | toHdr _ = raise Fail "Invalid header"
	  fun toReq [m, uri, ver] hdrs = { method=m, resource=uri, httpVersion=ver,
					   headers= map (fn h => toHdr (tokens ": " h)) hdrs,
					   parameters=[]}
	    | toReq _ _ = raise Fail "Invalid request line"
      in 
	  ((toReq (map sliceToStr (tokens " " req)) rest) : Request)
      end
  end
  end

	      
      (* "GET / HTTP/1.1\r\nHost: localhost:8184\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nCookie: __utma=111872281.1074254706.1427666251.1427666251.1427666251.1; __utmz=111872281.1427666251.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); C6gbrqqYAK3a1rKin6QaTAZDD5Oe0xnRat0RKe06ntufdcKUN12VtUXc8rfLrgw4\r\nConnection: keep-alive\r\n\r\n" *)
