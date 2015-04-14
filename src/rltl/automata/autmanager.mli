open Exptree
open Shared

type t =
  {
    mutable aut_size: int;
    aut_bddmgr: Bdd.manager;
    aut_expmgr: Manager.t;
    aut_ahwmgr: Ahw.Make(Nfa.Make(Bool.Default.B)).manager;
    aut_nbwmgr: Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).manager;
    aut_label: (Node.t, Bool.Default.B.t) Hashtbl.t;
    aut_nfa: (Node.t, Nfa.Make(Bool.Default.B).t) Hashtbl.t;
    aut_ahw: (Node.t, Ahw.Make(Nfa.Make(Bool.Default.B)).t) Hashtbl.t;
    aut_nbw: (Node.t, Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).t) Hashtbl.t;
  }

type label = Bool.Default.B.t
type nfa = Nfa.Make(Bool.Default.B).t
type ahw = Ahw.Make(Nfa.Make(Bool.Default.B)).t
type nbw  = Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).t

type automata =
| Nbw of nbw
| Ahw of ahw
| Nfa of nfa

val fullRank : ranking
val maxTwoRank : ranking
val stratifiedRank : ranking

val init : Manager.t -> t
val get_label : t -> Node.t -> Bool.Default.B.t
val get_nfa : t -> Node.t -> Nfa.Make(Bool.Default.B).t
val get_ahw : ?simpl:bool -> t -> Node.t -> Ahw.Make(Nfa.Make(Bool.Default.B)).t
val get_nbw : ?rank:ranking
  -> t -> Node.t -> Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).t
