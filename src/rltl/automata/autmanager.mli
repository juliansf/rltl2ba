open Exptree

(*
type t =
  {
    mutable aut_bddmgr: Bdd.manager;
    aut_label: (Node.t, bool_expr * Bdd.t option ref) Hashtbl.t;
    aut_nfa: (Node.t, regex_expr * Nfa.t option ref) Hashtbl.t;
    aut_abw: (Node.t, rltl_expr * Abw.t option ref) Hashtbl.t;
  }

val init: Manager.t -> t
*)

type t =
  {
    mutable aut_size: int;
    aut_bddmgr: Bdd.manager;
    aut_expmgr: Manager.t;
    aut_label: (Node.t, Bdd.t) Hashtbl.t;
    aut_nfa: (Node.t, Nfa.t) Hashtbl.t;
    aut_abw: (Node.t, Abw.t) Hashtbl.t;
  }

type label = Bdd.t
type nfa = Nfa.t

val init : Manager.t -> t
val get_label : t -> Node.t -> Bdd.t
val get_nfa : t -> Node.t -> Nfa.t
