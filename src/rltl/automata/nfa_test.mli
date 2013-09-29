module Bdd :
  sig
    type t = T | F | V of string | Not of t | Or of t * t | And of t * t
    val dnot : t -> t
    val dor : t -> t -> t
    val dand : t -> t -> t
    val is_false : t -> bool
  end


type trans = (Bdd.t * int) list
type t = {
  nfa_delta : trans array;
  nfa_start : int;
  nfa_final : bool array;
}
val size : t -> int
(*
val shift_k : int -> trans array -> trans array
val mark_forward_reachable :  t -> bool array
val mark_backward_reachable :  t -> bool array
val remove_unreachable : t -> t
val normalize : trans -> trans
val merge_states : t -> t
val reverse_delta : trans array -> trans array
val simplify : t -> t
*)

val nfa_false : t
val letter : Bdd.t -> t
val star : t -> t
val star2 : t -> t
val concat : t -> t -> t
val fusion : t -> t -> t
val plus : t -> t -> t
val product : t -> t -> t

val printf : Format.formatter -> t -> unit
val print : t -> unit

val a : t
val b : t
val c : t
val d : t

val ($^) : t -> t -> t
val ($:) : t -> t -> t
val ($&&) : t -> t -> t
val ($+) : t -> t -> t
