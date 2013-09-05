
type t =
| ValEntry of Expgen.node
| FunEntry of (t -> t)

val val_entry : t -> Expgen.node
(* [val_entry (FunEntry _)] raises a fatal error. *)

val fun_entry : t -> (t -> t)
(* [fun_entry (ValEntry _)] raises a fatal error. *)
