
(* Errors *)
exception Fatal_error
exception ValOf_None

val fatal_error: string -> 'a

val valOf: 'a option -> 'a

(* Hashtable functions *)
val create_hashtable: int -> ('a * 'b) list -> ('a,'b) Hashtbl.t
  (* Create a hashtable of the given size and fills it with the
     given bindings - form Ocaml.Misc module. *)

val uniques: 'a list -> 'a list
  (* Delete duplicates from a list, preserving the order of the
     first unique elements. *)

(* List comprehension *)
val range : int -> int -> int list

(* Statistics functions *)
val chrono : ('a -> 'b) -> 'a -> float * 'b
(* [chrono f x] evaluates [f x] and compute the elapsed time. *)

class timer : object
  method reset : unit
  method start : unit
  method stop : unit
  method value : float
end

val human_readable_byte_count : float -> string

(* Common modules *)
module IntSet : Set.S with type elt = int

module SetHash(S : Set.S) : Hashtbl.HashedType
  with type t = S.t

module SetHashtbl(S : Set.S) : Hashtbl.S
  with type key = S.t
