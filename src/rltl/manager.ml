type addr = int
type t =
  { env_id: int;
    env_vars: string list;
    env_tbl : (addr, Exptree.expression) Hashtbl.t;
    mutable env_lastref: addr;
  }

let last_envid = ref(-1)

let lookup env r =
  Hashtbl.find env.env_tbl r

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
    Exptree.iter_nodes (fun n ->  Exptree.link (lookup env n)) e;
    env.env_lastref
  end
  else node

let apply f env =
  Hashtbl.iter f env.env_tbl

let lastref env =
  env.env_lastref

let size env =
  Hashtbl.length env.env_tbl

let clean ?(keep_lastref=false) env =
  let rec unlink r =
    let x = lookup env r in begin
      Exptree.unlink x;
      if Exptree.links x <= 0 then
        Exptree.iter_nodes (fun n -> if n != r then unlink n) x
    end
  in
  Hashtbl.iter (fun r x ->
      if Exptree.links x <= 0 then
        if not keep_lastref || r != lastref env then
          Exptree.iter_nodes unlink x
    ) env.env_tbl;

  Hashtbl.iter (fun r x ->
      if Exptree.links x <= 0 then
        if not keep_lastref || r != lastref env then
          Hashtbl.remove env.env_tbl r
      ) env.env_tbl


let init () =
  incr last_envid;
  let env =
    {
      env_id = !last_envid;
      env_vars = [];
      env_tbl = Hashtbl.create 8;
      env_lastref = -1;
    } in
  ignore(Gc.create_alarm (fun () -> clean ~keep_lastref:true env));
  env
