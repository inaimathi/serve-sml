signature BUFFER =
  sig
      type Buffer
      val readInto : Buffer -> ('a,Socket.active Socket.stream) Socket.sock -> Buffer option
      val isTerminated : Buffer -> bool
      val test_isTerminated : unit -> Word8Array.elem * bool
      val new : int -> Buffer
  end 

structure DefaultBuffer : BUFFER =
  struct 
  type Buffer = { fill : int ref, buf : Word8Array.array }

  fun isTerminated {fill, buf} = 
      let fun chk c i = (Word8.fromInt (Char.ord c)) = (Word8Array.sub (buf, !fill - i))
      in 
	  (!fill >= 4) andalso (chk #"\r" 4) andalso (chk #"\n" 3) andalso (chk #"\r" 2) andalso (chk #"\n" 1)
      end

  fun test_isTerminated () =
      let val arr = map (Word8.fromInt o Char.ord) (String.explode "Testing \r\n\r\n ")
	  val b = { fill = ref 12, buf = Word8Array.fromList arr}
      in 
	  (Word8Array.sub (#buf b, !(#fill b)), isTerminated b)
      end

  fun new initSize = 
      { fill= ref 0, buf= Word8Array.array (initSize, Word8.fromInt 0) }

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
  end
