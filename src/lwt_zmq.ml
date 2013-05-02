module Socket = struct

  type 'a t = {
    socket : 'a ZMQ.Socket.t;
    fd : Lwt_unix.file_descr;
  }

  exception Break_event_loop

  let of_socket socket = {
    socket;
    fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false (ZMQ.Socket.get_fd socket);
  }

  let to_socket s = s.socket

  (* This is horribly convoluted but matches what the zeromq authors call for *)
  let wrap f s =
    let io_loop () =
      Lwt_unix.wrap_syscall Lwt_unix.Read s.fd (
        fun () ->
          try
            (* Check for zeromq events *)
            match ZMQ.Socket.events s.socket with
            | ZMQ.Socket.No_event -> raise Lwt_unix.Retry
            | ZMQ.Socket.Poll_in
            | ZMQ.Socket.Poll_out
            | ZMQ.Socket.Poll_in_out -> f s.socket
          with
          (* Not ready *)
          | ZMQ.ZMQ_exception (ZMQ.EAGAIN, _) -> raise Lwt_unix.Retry
          (* We were interrupted so we need to start all over again *)
          | ZMQ.ZMQ_exception (ZMQ.EINTR, _) -> raise Break_event_loop
      )
    in
    let rec idle_loop () =
      try_lwt
        Lwt.wrap1 f s.socket
      with
      | ZMQ.ZMQ_exception (ZMQ.EAGAIN, _) -> begin
        try_lwt
          io_loop ()
        with
        | Break_event_loop -> idle_loop ()
      end
      | ZMQ.ZMQ_exception (ZMQ.EINTR, _) ->
          idle_loop ()
    in
    idle_loop ()

  let recv s =
    wrap (fun s -> ZMQ.Socket.recv ~opt:ZMQ.Socket.R_no_block s) s

  let send s m =
    wrap (fun s -> ZMQ.Socket.send ~opt:ZMQ.Socket.S_no_block s m) s

  let recv_all s =
    (* Once the first piece arrives we don't need to worry about the rest
       blocking *)
    lwt first =
      wrap (fun s -> ZMQ.Socket.recv ~opt:ZMQ.Socket.R_no_block s) s
    in
    let rec loop accu =
      if ZMQ.Socket.has_more s.socket then
        loop (ZMQ.Socket.recv s.socket :: accu)
      else
        accu
    in
    Lwt.return (List.rev (loop [first]))

  let send_all s parts =
    match parts with
    | [] -> Lwt.return_unit
    | hd :: [] -> send s hd
    | hd :: tl ->
        (* Once the first piece sends we don't need to worry about the rest
           blocking *)
        lwt () =
          wrap (fun s -> ZMQ.Socket.send ~opt:ZMQ.Socket.S_more_no_block s hd) s
        in
        let rec loop remaining =
          match remaining with
          | [] -> Lwt.return_unit (* This shouldn't happen *)
          | p :: [] ->
              (* This is the last piece *)
              ZMQ.Socket.send s.socket p;
              Lwt.return_unit
          | p :: r ->
              (* There is still more to send after this *)
              ZMQ.Socket.send ~opt:ZMQ.Socket.S_more s.socket p;
              loop r
        in
        loop tl
end

