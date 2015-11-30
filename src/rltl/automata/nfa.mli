module type S = sig
  type error =
  | Invalid_Nfa

  exception Error of error

  module Label : Bool.S

  type label = Label.t
  type trans = (label * int) list
  type t =
    { nfa_delta: trans array;
      nfa_start: int;
      nfa_final: bool array;
      nfa_scc: (int array) array;
    }

  val size : t -> int

  val nfa_false : t
  val letter : label -> t
  val star : t -> t
  val concat : t -> t -> t
  val fusion : t -> t -> t
  val plus : t -> t -> t
  val product : t -> t -> t

  val tarjanSCC : trans array -> (int array) array
end

module Make(B : Bool.S) : S with module Label = B
