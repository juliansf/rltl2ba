
type error =
| Expression_type_clash of Types.type_expr * Types.type_expr
| Expression_type_wrong of Types.type_expr * Types.type_expr list
| Too_many_arguments
| Builtin_function of string
| Type_undefined of string

exception Error of error * Location.t

val expression: Typeenv.t -> Parsetree.expression -> Typedtree.expression

val report_error : Format.formatter -> error -> unit
