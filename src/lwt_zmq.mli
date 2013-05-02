module Socket : sig

  (** An Lwt-wrapped zeromq socket *)
  type 'a t

  (** [of_socket s] wraps the zeromq socket [s] for use with Lwt *)
  val of_socket : 'a ZMQ.Socket.t -> 'a t

  (** [to_socket s] extracts the raw zeromq socket from [s] *)
  val to_socket : 'a t -> 'a ZMQ.Socket.t

  (** [recv socket] waits for a message on [socket] without blocking other Lwt
      threads *)
  val recv : 'a t -> string Lwt.t

  (** [send socket m] sends a message [m] on [socket] without blocking other
      Lwt threads *)
  val send : 'a t -> string -> unit Lwt.t

  (** [recv_all socket] waits for a multi-part message on [socket] without
      blocking other Lwt threads *)
  val recv_all : 'a t -> string list Lwt.t

  (** [send_all socket m] sends all parts of the multi-part message [m] on
      [socket] without blocking other Lwt threads *)
  val send_all : 'a t -> string list -> unit Lwt.t
end
