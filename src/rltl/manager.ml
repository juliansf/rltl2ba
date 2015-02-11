type addr = int
type t =
  { env_id: int;
    env_vars: string list;
    env_tbl : (addr, Exptree.expression) Hashtbl.t;
    mutable env_lastref: addr;
  }

let last_envid = ref(-1)

let init () =
  incr last_envid;
  {
    env_id = !last_envid;
    env_vars = [];
    env_tbl = Hashtbl.create 8;
    env_lastref = -1;
  }

let add env e =
  let f k x res =
    if res < 0 then
      if Exptree.equal_expr e x then k else -1
    else res
  in
  let node = Hashtbl.fold f env.env_tbl (-1) in
  if node < 0 then begin
    env.env_lastref <- env.env_lastref+1;
    Hashtbl.add env.env_tbl env.env_lastref e;
    env.env_lastref
  end
  else node

let lookup env r = Hashtbl.find env.env_tbl r

let apply f env = Hashtbl.iter f env.env_tbl

let lastref env = env.env_lastref

let size env = Hashtbl.length env.env_tbl
