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
end

module Result = struct
  type t = { 
    worker_id: int;
    name : string;
    completion_time : int 
  }
  let serialize (j : t) = Marshal.to_string j []
  let deserialize s = Marshal.from_string s 0
end
