
type addr = Node.t
type t

val init: unit -> t
val add: t -> Exptree.expression -> addr
val lookup: t -> addr -> Exptree.expression
val apply: (addr -> Exptree.expression -> unit) -> t -> unit
val lastref: t -> addr
val size: t -> int
