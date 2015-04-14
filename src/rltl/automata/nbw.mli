module type S = sig
  type error =
  | Invalid_Nbw

  exception Error of error

  module Ahw : Ahw.S

  type ranking = Shared.ranking

  type state = { s : int array; o : bool array; f : int array; ok : bool; }
  type trans = (state, Ahw.Nfa.Label.t) Hashtbl.t
  type reference = int

  type nbw =
    {
      nbw_delta : (state,trans) Hashtbl.t;
      nbw_init: (state,unit) Hashtbl.t;
    }

  type manager =
    { nbw_ahwmgr: Ahw.manager;
      nbw_aut_number: int ref;
      nbw_automata: (reference, nbw) Hashtbl.t;
    }

  type t = reference

  val _true: state

  val init: Ahw.manager -> manager

  val from_ahw: ?rank:ranking -> manager -> Ahw.t -> t
  val find: manager -> reference -> nbw
  val remove: manager -> reference -> unit

  val is_false: nbw -> bool
  val is_true: nbw -> bool

  (* Auxiliary functions *)
  val is_final: nbw -> state -> bool
  val get_delta: nbw -> state -> trans
end

module Make(A : Ahw.S) : S with module Ahw = A
