
type t = Cudd.node
type addr = int

type manager =
  { bdd_mgr: Cudd.manager;
    bdd_tbl: (addr, t) Hashtbl.t;
    bdd_vars: (string, t) Hashtbl.t;
  }

val init: unit -> manager
val named_var: manager -> string -> t
val indexed_vars: manager -> string array
val hashed_vars: manager -> (int,string) Hashtbl.t
val manager_apply: (addr -> t -> unit) -> manager -> unit
