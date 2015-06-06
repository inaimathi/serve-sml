(* ***** Basic Utility *)
fun fst (a, _) = a
fun snd (_, b) = b

fun consOpt NONE lst = lst
  | consOpt (SOME e) lst = e::lst

(* ***** Toy server *)
fun sendHello sock = 
    let val res = "HTTP/1.1 200 OK\r\nContent-Length: 14\r\n\r\nHello from ML!\r\n\r\n"
	val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in 
	print "Sending...\n";
	Socket.sendVec (sock, slc);
	Socket.close sock;
	NONE		     
    end

fun processClients _ [] acc = acc
  | processClients [] rest acc = rest @ acc
  | processClients (d::ds) (s::ss) acc = if d = (Socket.sockDesc s)
					 then processClients ds ss (consOpt (sendHello s) acc)
					 else processClients (d::ds) ss acc

fun processServers _ [] acc = acc
  | processServers [] _ acc = acc
  | processServers (d::ds) (s::ss) acc = if d = (Socket.sockDesc s)
					 then processServers ds ss ((fst (Socket.accept s))::acc)
					 else processServers (d::ds) ss acc


fun descs ss = map Socket.sockDesc ss

fun selecting server clients timeout =
    let val { rds, exs, wrs } = Socket.select {
		rds = (Socket.sockDesc server) :: (descs clients),
		wrs = [], exs = [], timeout = timeout
	    }
    in
	rds
    end

fun acceptLoop serv clients =
    let val ready = selecting serv clients NONE
	val newCs = processServers ready [serv] []
	val next = processClients ready clients []
    in
	acceptLoop serv (newCs @ next)
    end

fun serve port =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any port);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s []
    end
