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

fun serve () =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any 8989);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s
    end
