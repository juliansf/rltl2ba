
val free_vars: Typedtree.expression -> (Ident.t*int) list

val expression
  : Types.type_expr -> Typedtree.expression -> Expgen.manager * Expgen.node

val print_expr : Format.formatter -> Expgen.manager * Expgen.node -> unit
