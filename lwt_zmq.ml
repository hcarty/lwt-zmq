module Lwt_socket = struct
  type 'a t = {
    socket : 'a ZMQ.Socket.t;
    fd : Lwt_unix.file_descr;
  }

  let of_socket socket = {
    socket;
    fd = Lwt_unix.of_unix_file_descr (ZMQ.Socket.get_fd socket);
  }

  (** [event_in e] returns [true] if [e] indicates that a message can be
      received without blocking. *)
  let event_in = function
    | ZMQ.Socket.Poll_in
    | ZMQ.Socket.Poll_in_out -> true
    | ZMQ.Socket.Poll_out
    | ZMQ.Socket.No_event -> false

  (** [event_out e] returns [true] if [e] indicates that a message can be sent
      without blocking. *)
  let event_out = function
    | ZMQ.Socket.Poll_out
    | ZMQ.Socket.Poll_in_out -> true
    | ZMQ.Socket.Poll_in
    | ZMQ.Socket.No_event -> false

  let rec recv s =
    (* Wait for the socket to be readable *)
    lwt () = Lwt_unix.wait_read s.fd in
    (* Check to make sure we have a message waiting *)
    if event_in (ZMQ.Socket.events s.socket) then (
      (* We can receive a message without blocking - do it *)
      Lwt.return (ZMQ.Socket.recv s.socket)
    )
    else (
      (* No relevant event ready, so loop and try again *)
      recv s
    )

  let rec send s m =
    (* Wait for the socket to be writable *)
    lwt () = Lwt_unix.wait_write s.fd in
    (* Check to make sure we can send a message without blocking *)
    if event_out (ZMQ.Socket.events s.socket) then (
      Lwt.return (ZMQ.Socket.send s.socket m)
    )
    else (
      send s m
    )
end
