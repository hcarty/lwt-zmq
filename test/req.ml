let () =
  let z = ZMQ.Context.create () in
  let socket = ZMQ.Socket.create z ZMQ.Socket.req in
  ZMQ.Socket.connect socket "tcp://127.0.0.1:5555";
  Lwt_main.run (
    let lwt_socket = Lwt_zmq.Socket.of_socket socket in
    lwt () = Lwt_io.printl "Sending a request" in
    lwt () = Lwt_zmq.Socket.send lwt_socket "request" in
    lwt () = Lwt_io.printl "Request sent, waiting for reply" in
    lwt reply = Lwt_zmq.Socket.recv lwt_socket in
    lwt () = Lwt_io.printlf "Received: %s" reply in
    assert_lwt (reply = "reply")
  );
  ZMQ.Socket.close socket;
  ZMQ.Context.terminate z;
  ()

