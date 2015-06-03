(* ***** Basic Utility *)
fun each f [] = ()
  | each f lst = let val (ln::lns) = lst
		 in f ln; 
		    each f lns
		 end 

fun stringToVecSlice str = 
    Word8VectorSlice.full (Byte.stringToBytes str)

(* ***** Socket-related utility *)
val crlf = stringToVecSlice "\r\n"

fun sendLine (sock, str) = 
    let in 
	Socket.sendVec(sock, str);
	Socket.sendVec(sock, crlf)
    end

fun sendLines (sock, lns) = each (fn ln => sendLine (sock, ln)) lns

fun sendString (sock, str) = 
    Socket.sendVec(sock, stringToVecSlice str)

(* ***** Toy server *)
fun sendHello sock = 
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nHello world!\r\n\r\n"
	val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in 
	Socket.sendVec (sock, slc);
	Socket.close sock
    end

fun acceptLoop serv =
    let val (s, _) = Socket.accept serv
    in print "Accepted a connection...\n";
       sendHello s;
       acceptLoop serv
    end

fun serve port =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any port);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s
    end
