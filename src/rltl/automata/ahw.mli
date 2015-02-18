module type S = sig
  type error =
  | Invalid_Ahw

  exception Error of error

  module Nfa : Nfa.S

  type state = int
  type trans = Nfa.Label.t

  type strata = SAccept | SReject | SBuchi | SCoBuchi

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_color: (state, int) Hashtbl.t;
      (*ahw_strata: (int, strata) Hashtbl.t;*)
      ahw_false: state;
      ahw_true: state;
    }

  type ahw = state

  type t =
    {ahw_regular: ahw;
     ahw_dual: ahw;
    }

  val init: Bdd.manager -> manager

  val empty: manager -> t
  val letter: manager -> Nfa.label -> t
  val negate: t -> t
  val disj: manager -> t -> t -> t
  val conj: manager -> t -> t -> t
  val concat: manager -> Nfa.t -> t -> t
  val univ_concat: manager -> Nfa.t -> t -> t
  val fusion: manager -> Nfa.t -> t -> t
  val univ_fusion: manager -> Nfa.t -> t -> t
  val power: manager -> t -> Nfa.t -> t -> t
  val dual_power: manager -> t -> Nfa.t -> t -> t
  val weak_power: manager -> t -> Nfa.t -> t -> t
  val dual_weak_power: manager -> t -> Nfa.t -> t -> t
  val power_fusion: manager -> t -> Nfa.t -> t -> t
  val dual_power_fusion: manager -> t -> Nfa.t -> t -> t
  val weak_power_fusion: manager -> t -> Nfa.t -> t -> t
  val dual_weak_power_fusion: manager -> t -> Nfa.t -> t -> t
  val closure: manager -> Nfa.t -> t
  val dual_closure: manager -> Nfa.t -> t

  (*val simplify: manager -> t -> unit*)

  (*
    val univ_of_nfa: manager -> Nfa.t -> state * int list
    val exist_of_nfa: manager -> Nfa.t -> state * int list
  *)

  (* Auxiliary functions *)
  val get_delta: manager -> state -> trans
  val get_color: manager -> state -> int
end


module Make(N : Nfa.S) : S with module Nfa = N












(*
  #directory "lib/cudd";;
  #directory "src/rltl/automata/";;
  #load "cudd.cma";;
  #load_rec "printahw.cmo";;
  #load_rec "printnfa.cmo";;
  #load_rec "printbdd.cmo";;

  let bm = Bdd.init ();;

  let a = Bdd.named_var bm "a";;
  let b = Bdd.named_var bm "b";;
  let c = Bdd.named_var bm "c";;
  let d = Bdd.named_var bm "d";;

  let am = Ahw.init bm;;

  let shownfa = Printnfa.print_nfa (Printbdd.print_node bm) Format.std_formatter;;
  let showahw = Printahw.print_ahw am Format.std_formatter;;
  let showregular ahw = showahw ahw.Ahw.ahw_regular;;
  let showdual ahw = showahw ahw.Ahw.ahw_dual;;

  let ($&&) = Nfa.product;;
  let ($||) = Nfa.plus;;
  let ($;) = Nfa.concat;;
  let ( $* ) x () = Nfa.star x;;

  let ln s = Nfa.letter (Bdd.named_var bm s);;
  let la s = Ahw.letter am (Bdd.named_var bm s);;

  let n_a = Nfa.letter a;;
  let n_b = Nfa.letter b;;
  let n_c = Nfa.letter c;;
  let n_d = Nfa.letter d;;

  let _a = Ahw.letter am a;;
  let _b = Ahw.letter am b;;
  let _c = Ahw.letter am c;;
  let _d = Ahw.letter am d;;

  let tt = Ahw.negate (Ahw.empty am);;
  let r0 = Nfa.star (Nfa.product (Nfa.concat n_a (Nfa.star n_b)) (Nfa.star n_c));;
  let r1 = Nfa.plus (Nfa.concat (Nfa.plus n_a n_b) n_b) n_c;;

  let r2 = Nfa.star (Nfa.concat n_a (Nfa.concat n_b (Nfa.star n_c)));;

  let r = n_c;;
  let r = Nfa.star n_c;;
  let cd = Ahw.concat am r _d;;
  let ucd = Ahw.univ_concat am r _d;;
  let cdf = Ahw.fusion am r _d;;
  let ucdf = Ahw.univ_fusion am r _d;;
*)
