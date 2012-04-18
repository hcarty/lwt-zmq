module Socket = struct
  type 'a t = {
    socket : 'a ZMQ.Socket.t;
    fd : Lwt_unix.file_descr;
  }

  let of_socket socket = {
    socket;
    fd = Lwt_unix.of_unix_file_descr ~blocking:false (ZMQ.Socket.get_fd socket);
  }

  let wrap event f s =
    Lwt_unix.wrap_syscall event s.fd (
      fun () ->
        try
          f s.socket
        with
        | ZMQ.ZMQ_exception (ZMQ.EAGAIN, _) -> raise Lwt_unix.Retry
    )

  let recv s =
    wrap Lwt_unix.Read (fun s -> ZMQ.Socket.recv ~opt:ZMQ.Socket.R_no_block s) s

  let send s m =
    wrap Lwt_unix.Write (fun s -> ZMQ.Socket.send ~opt:ZMQ.Socket.S_no_block s m) s
end

