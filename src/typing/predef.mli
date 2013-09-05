
open Types

val type_bool: type_expr
val type_regex: type_expr
val type_rltl: type_expr

val default_type: type_expr

val add_builtin_types : unit -> unit
val build_type_pairs : unit -> unit

val build_initial_env:
  (Ident.t -> type_expr -> 'a -> 'a) -> 'a -> 'a
