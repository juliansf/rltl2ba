
type t

type minmod = True | False | Mod of int list list

type manager =
  { plus_mgr: Cudd.manager; }

val init: unit -> manager

val dtrue: manager -> t
val dfalse: manager -> t
val ithvar: manager -> int -> t
val index: t -> int
val is_true: t -> bool
val is_false: t -> bool
val dor: t -> t -> t
val dand: t -> t -> t

val min_models: t -> minmod
val shift_by: manager -> int -> t -> t
val map: manager -> (int -> int) -> t -> t
