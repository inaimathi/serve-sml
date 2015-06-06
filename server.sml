(* ***** Basic Utility *)
fun each f [] = ()
  | each f lst = let val (ln::lns) = lst
		 in f ln; 
		    each f lns
		 end 

fun stringToVecSlice str = 
    Word8VectorSlice.full (Byte.stringToBytes str)

fun fst (a, _) = a
fun snd (_, b) = b

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
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 14\r\n\r\nHello from ML!\r\n\r\n"
	val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in 
	print "Sending...\n";
	Socket.sendVec (sock, slc);
	Socket.close sock
    end

(* fun processClient sock = sendHello sock; NONE *)

fun descs ss = map Socket.sockDesc ss

fun selecting server clients =
    let val { rds, exs, wrs } = Socket.select {
		rds = descs [server],
		wrs = [],
		exs = descs clients,
		timeout = NONE
	    }
    in
	(rds, exs)
    end

(* fun acceptLoop serv clients = *)
(*     let val (ss, cs) = selecting serv clients NONE *)
(* 	val newCs = if ss = [] then [] else [Socket.accept serv] *)
(* 	val next = processClients cs clients *)
(*     in  *)
(* 	sendHello c; *)
(* 	acceptLoop serv clients *)
(*     end *)

fun acceptLoop serv =
    let val (c, _) = Socket.accept serv
    in 
	sendHello c;
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
