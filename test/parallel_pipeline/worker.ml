open Protocol
open Lwt

let () = Random.self_init ()

let z = ZMQ.Context.create ()

let lwt_zmq_socket z ~zmq_type ~address =
  let s = ZMQ.Socket.create z zmq_type in
  ZMQ.Socket.connect s address;
  (s, Lwt_zmq.Socket.of_socket s)

let complete_job { Job.name; estimated_time } =
  let time_diff = (Random.int Job.min_time) + Job.max_time in
  (fun ~worker_id -> 
    { Result.name; worker_id; completion_time=(estimated_time + time_diff) })

let run_worker ~worker_id =
  let module ZM = ZMQ.Socket in
  let (s1, pull) = lwt_zmq_socket z ~zmq_type:ZM.pull ~address:Address.job in
  let (s2, push) = lwt_zmq_socket z ~zmq_type:ZM.push ~address:Address.result in
  ignore (Lwt_io.printf "Created push/pull pair for worker %d\n" worker_id);
  let rec loop () =
    Lwt_zmq.Socket.recv pull
    >>= (fun msg ->
      let job = Result.deserialize msg in 
      let completed_job = (complete_job job) ~worker_id in
      (* We simulate doing actual work by sleeping. Normally this call would
       * be CPU intensive so taking on a job at a time is fine *)
      Lwt_unix.sleep (float_of_int completed_job.Result.completion_time)
      >>= (fun () -> return completed_job))
    >>= (fun completed_job ->
      let result = Result.serialize completed_job in
      (Lwt_io.printf "Worker %d completed job '%s.\n" worker_id completed_job.Result.name)
      >>= (fun () -> Lwt_zmq.Socket.send push result))
    >>= loop 
  in loop ()

(* note that we are being a little lazy and not cleaning up resources 
 * because we are in an infinite loop anyway *)
let () = Lwt_main.run ( run_worker ~worker_id:(Random.int 100000) )

