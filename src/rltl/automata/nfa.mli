type error =
| Invalid_Nfa

exception Error of error

type trans = (Bdd.t * int) list
type t =
  { nfa_delta: trans array;
    nfa_start: int;
    nfa_final: bool array;
  }

val size : t -> int

val nfa_false : t
val letter : Bdd.t -> t
val star : t -> t
val concat : t -> t -> t
val fusion : t -> t -> t
val plus : t -> t -> t
val product : t -> t -> t
