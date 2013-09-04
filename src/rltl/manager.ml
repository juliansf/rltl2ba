type addr = int
type t =
  { env_id: int;
    mutable env_tbl : (addr, Exptree.expression) Hashtbl.t;
    mutable env_lastref: addr;
  }

let last_envid = ref(-1)

let init () =
  incr last_envid;
  {
    env_id = !last_envid;
    env_tbl = Hashtbl.create 8;
    env_lastref = -1;
  }

let add env e =
  env.env_lastref <- env.env_lastref+1;
  Hashtbl.add env.env_tbl env.env_lastref e;
  env.env_lastref

let lookup env r = Hashtbl.find env.env_tbl r

let apply f env = Hashtbl.iter f env.env_tbl

let lastref env = env.env_lastref
