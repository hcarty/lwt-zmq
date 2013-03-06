
let random_str ~len = 
  let rand_chr () = (Char.chr (97 + (Random.int 26))) in
  let s = String.make len 'a' in
  String.map (fun _ -> rand_chr ()) s

module Address = struct
  let job = "tcp://127.0.0.1:6558"
  let result = "tcp://127.0.0.1:6557"
end

module Job = struct
  type t = {
    name : string;
    estimated_time : int 
  }
  let serialize (j : t) = Marshal.to_string j []
  let deserialize s = Marshal.from_string s 0

  let jobs_per_second = 1

  let (min_time,max_time) = (1, 2)

  let random_job () =
    let time = (Random.int max_time) + min_time in
    { estimated_time=time; name=(random_str ~len:8) }
end

module Result = struct
  type t = { 
    worker_id: int;
    name : string;
    completion_time : int 
  }
  let serialize (j : t) = Marshal.to_string j []
  let deserialize s = Marshal.from_string s 0

  let (min_time,max_time) = (2,4)
end
