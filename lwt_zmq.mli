module Lwt_socket : sig

  (** An Lwt-wrapped zeromq socket *)
  type 'a t

  (** [of_socket s] wraps the zeromq socket [s] for use with Lwt *)
  val of_socket : 'a ZMQ.Socket.t -> 'a t

  (** [recv socket] waits for a message on [socket] without blocking other Lwt threads *)
  val recv : 'a t -> string Lwt.t

  (** [send socket] sends a message on [socket] without blocking other Lwt threads *)
  val send : 'a t -> string -> unit Lwt.t
end
