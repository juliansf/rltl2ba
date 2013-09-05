
val newty: Types.type_desc -> Types.type_expr
  (* Create a generic type *)

val type_of_string: string -> Types.type_expr

val add_builtin: string -> Types.type_expr -> unit
val add_type_pair: Types.type_expr * Types.type_expr -> unit

val subtype: Types.type_expr -> Types.type_expr -> bool
