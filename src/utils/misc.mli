
(* Errors *)
exception Fatal_error
exception ValOf_None

val fatal_error: string -> 'a

val valOf: 'a option -> 'a

val create_hashtable: int -> ('a * 'b) list -> ('a,'b) Hashtbl.t
  (* Create a hashtable of the given size and fills it with the
     given bindings - form Ocaml.Misc module. *)
