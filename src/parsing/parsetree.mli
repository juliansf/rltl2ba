open Asttypes

type ty = {
  pty_desc: ty_desc;
  pty_loc: Location.t;
}

and ty_desc =
| Pty_name of string loc
| Pty_arrow of ty * ty

and var = {
  pvar_desc: var_desc;
  pvar_loc: Location.t;
}

and var_desc =
| Pvar_ident of string loc
| Pvar_funct of string loc * (string loc * ty) list

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
| Pexp_boolconst of string
| Pexp_ident of string
| Pexp_apply of expression * expression list
| Pexp_power of power_flag * expression * expression * expression option
| Pexp_overlap of expression
| Pexp_closure of expression
| Pexp_let of (var * expression) * expression
