(* Compile a file *)

val expression_file : Types.type_expr -> Format.formatter -> string -> unit
val expression_string : Format.formatter -> string -> unit
val _file : Format.formatter -> (Lexing.lexbuf -> 'a) -> string -> 'a
val _string : Format.formatter -> (Lexing.lexbuf -> 'a) -> string -> 'a
