let () =
  let z = ZMQ.init () in
  let socket = ZMQ.Socket.create z ZMQ.Socket.rep in
  ZMQ.Socket.bind socket "tcp://127.0.0.1:5555";
  Lwt_main.run (
    let lwt_socket = Lwt_zmq.Socket.of_socket socket in
    lwt () = Lwt_io.printl "Waiting for a message" in
    lwt request = Lwt_zmq.Socket.recv lwt_socket in
    lwt () = Lwt_io.printlf "Received: %s, sending a reply" request in
    lwt () = assert_lwt (request = "request") in
    lwt () = Lwt_zmq.Socket.send lwt_socket "reply" in
    Lwt_io.printl "Reply sent"
  );
  ZMQ.Socket.close socket;
  ZMQ.term z;
  ()

