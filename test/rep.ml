let (>>=) = Lwt.(>>=)

let () =
  let z = ZMQ.Context.create () in
  let socket = ZMQ.Socket.create z ZMQ.Socket.rep in
  ZMQ.Socket.bind socket "tcp://127.0.0.1:5555";
  Lwt_main.run (
    let lwt_socket = Lwt_zmq.Socket.of_socket socket in
    Lwt_io.printl "Waiting for a message" >>= fun () ->
    Lwt_zmq.Socket.recv lwt_socket >>= fun request ->
    Lwt_io.printlf "Received: %s, sending a reply" request >>= fun () ->
    assert (request = "request");
    Lwt_zmq.Socket.send lwt_socket "reply" >>= fun () ->
    Lwt_io.printl "Reply sent"
  );
  ZMQ.Socket.close socket;
  ZMQ.Context.terminate z
