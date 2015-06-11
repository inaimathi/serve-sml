datatype BufferStatus = Complete | Incomplete | Errored

signature BUFFER =
  sig
      type Buffer
      val readInto : Buffer -> ('a,Socket.active Socket.stream) Socket.sock -> BufferStatus
      val new : int -> Buffer
      val toSlice : Buffer -> Word8ArraySlice.slice
      val printBuffer : Buffer -> unit
  end 

structure DefaultBuffer : BUFFER =
  struct 
  type Buffer = { fill : int ref, buf : Word8Array.array }
  local 
      fun isTerminated {fill, buf} = 
	  let fun chk c i = (Word8.fromInt (Char.ord c)) = (Word8Array.sub (buf, !fill - i))
	  in 
	      (!fill >= 4) andalso (chk #"\r" 4) andalso (chk #"\n" 3) andalso (chk #"\r" 2) andalso (chk #"\n" 1)
	  end
  in 
  fun readInto (buffer : Buffer) sock = 
      let val i = Socket.recvArrNB (sock, Word8ArraySlice.slice (#buf buffer, !(#fill buffer), SOME 1))
	  fun bump NONE = ()
	    | bump (SOME n) = (#fill buffer := (!(#fill buffer) + n); ())
      in 
	  bump i;
	  if isTerminated buffer
	  then Complete
	  else if i = NONE
	  then Incomplete
	  else readInto buffer sock
      end
      handle _ => Errored

  fun new initSize = 
      { fill= ref 0, buf= Word8Array.array (initSize, Word8.fromInt 0) }

  fun toSlice {fill, buf} = Word8ArraySlice.slice (buf, 0, SOME (!fill))

  fun printBuffer buffer =
      let fun printByte bt = 
	      let val chr = (Char.chr (Word8.toInt bt))
	      in 
		  print (Char.toString chr);
		  if chr = #"\n" then print "\n   " else print "";
		  ()
	      end 
      in
	  print "\n   ";
	  Word8ArraySlice.app printByte (toSlice buffer);
	  ()
      end
  end
end
