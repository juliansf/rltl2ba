module type S = sig
  type error =
  | Invalid_Nbw

  exception Error of error

  module Ahw : Ahw.S

  type ranking =
  | StratifiedRank
  | MaxTwoRank
  | FullRank

  type state = Misc.IntSet.t
  type trans = (state, Ahw.Nfa.Label.t) Hashtbl.t

  type manager =
    { nbw_ahwmgr: Ahw.manager;
      nbw_delta: (state, trans) Hashtbl.t;
      nbw_final: (state, unit) Hashtbl.t;
      nbw_true: state;
    }

  type nbw = state
  type t = nbw

  val init: Ahw.manager -> manager

  val from_ahw: ?rank:ranking -> manager -> Ahw.t -> t


  (* Auxiliary functions *)
  val is_final: manager -> state -> bool
  val get_delta: manager -> state -> trans
end

module Make(A : Ahw.S) : S with module Ahw = A
