val print_type: Format.formatter ->  Types.type_expr -> unit

val report_unification_error:
  Format.formatter -> (Types.type_expr * Types.type_expr list) ->
  (Format.formatter -> unit) -> (Format.formatter -> unit) -> unit
