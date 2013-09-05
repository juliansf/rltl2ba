open Asttypes
open Types

type ty =
  { ty_desc: type_expr;
    ty_loc: Location.t;
    ty_env: Typeenv.t}

and var =
  { var_desc: var_desc;
    var_loc: Location.t;
    var_type: type_expr;
    mutable var_env: Typeenv.t }

and var_desc =
| Tvar_ident of string loc
| Tvar_funct of string loc * (string loc * ty) list

and expression =
  { texp_desc: expression_desc;
    texp_loc: Location.t;
    texp_type: type_expr;
    texp_env: Typeenv.t }

and expression_desc =
| Texp_boolconst of string
| Texp_ident of string
| Texp_apply of expression * expression list
| Texp_power of power_flag * expression * expression * expression option
| Texp_overlap of expression
| Texp_closure of expression
| Texp_let of (var * expression) * expression

(* Auxiliary functions over the ast *)

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc
