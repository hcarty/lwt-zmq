open Protocol
open Lwt

module ZM = ZMQ.Socket

let z = ZMQ.init ()

let lwt_zmq_socket z ~zmq_type ~address = 
  let s = ZMQ.Socket.create z zmq_type in
  ZMQ.Socket.bind s address;
  (s, Lwt_zmq.Socket.of_socket s)

let work_creator ~rate = (* ~rate jobs a second *)
  let (s, push) = lwt_zmq_socket z ~zmq_type:ZM.push ~address:Address.job in
  let rec loop () = 
    let jobs = Array.(to_list (init rate 
            (fun _ -> Job.random_job () ))) in
    (Lwt_list.iter_p (fun job -> 
       Lwt_zmq.Socket.send push (Job.serialize job)) jobs)
    >>= (fun () -> Lwt_io.printf "Scheduled %d jobs\n" rate)
    >>= (fun () -> Lwt_unix.sleep (float_of_int rate))
    >>= loop
  in loop ()

let print_result {Result.worker_id; name; completion_time} =
  Lwt_io.printf "'%d' finished task '%s' in %d seconds\n"
    worker_id name completion_time

let results_collector () = 
  let (s, pull) = lwt_zmq_socket z ~zmq_type:ZM.pull ~address:Address.result in
  let rec loop () =
    Lwt_zmq.Socket.recv pull 
    >>= (fun msg -> return (Result.deserialize msg))
    >>= print_result
    >>= loop
  in loop ()

(* note that we are being a little lazy and not cleaning up resources 
 * because we are in an infinite loop anyway *)
let () = Lwt_main.run ( 
    Lwt.join [work_creator ~rate:Job.jobs_per_second;
              results_collector ()] )
