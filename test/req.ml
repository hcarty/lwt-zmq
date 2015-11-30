let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let () =
  let z = ZMQ.Context.create () in
  let socket = ZMQ.Socket.create z ZMQ.Socket.req in
  ZMQ.Socket.connect socket "tcp://127.0.0.1:5555";
  Lwt_main.run (
    let lwt_socket = Lwt_zmq.Socket.of_socket socket in
    Lwt_io.printl "Sending a request" >>= fun () ->
    Lwt_zmq.Socket.send lwt_socket "request" >>= fun () ->
    Lwt_io.printl "Request sent, waiting for reply" >>= fun () ->
    Lwt_zmq.Socket.recv lwt_socket >>= fun reply ->
    Lwt_io.printlf "Received: %s" reply >|= fun () ->
    assert (reply = "reply")
  );
  ZMQ.Socket.close socket;
  ZMQ.Context.terminate z
