open Btype
open Types

let type_unit = newty (Tvar (Some "unit"))

let type_bool = newty (Tvar (Some "bool"))

let type_regex = newty (Tvar (Some "regex"))

let type_rltl = newty (Tvar (Some "rltl"))

let default_type = type_bool

let add_builtin_types () = begin
  Btype.add_builtin "unit" type_unit;
  Btype.add_builtin "bool" type_bool;
  Btype.add_builtin "regex" type_regex;
  Btype.add_builtin "rltl" type_rltl;
end

let build_type_pairs () = begin
  Btype.add_type_pair (type_bool, type_regex);
  Btype.add_type_pair (type_bool, type_rltl);
  Btype.add_type_pair (type_regex, type_rltl);
end

let mk_binop (l,r) res =
  newty (Tarrow (l, (newty (Tarrow (r, res)))))

let poly_binop (l,r) res =
  newty(Tarrow (l, newty (
      Tpoly (r, res, fun t ->
        if Btype.subtype res t then t else res))))

let build_initial_env add_builtin empty_env =
  let builtin_functions = [
    (* Bool operators *)
    "!", newty(Tpoly (type_rltl, type_rltl, fun t ->
      if Btype.subtype t type_bool then type_bool
      else type_rltl));
    (*newty (Tarrow (type_bool, type_bool));*)
    "&", mk_binop (type_bool, type_bool) type_bool;
    "|", mk_binop (type_bool, type_bool) type_bool;
    "=", mk_binop (type_bool, type_bool) type_bool;
    "!=", mk_binop (type_bool, type_bool) type_bool;
    "->", mk_binop (type_bool, type_bool) type_bool;
    "<->", mk_binop (type_bool, type_bool) type_bool;

    (* Regex opeators *)
    "*", newty (Tarrow (type_regex, type_regex));
    "+", mk_binop (type_regex, type_regex) type_regex;
    "||", mk_binop (type_regex, type_regex) type_regex;
    "&&", mk_binop (type_regex, type_regex) type_regex;
    ".", mk_binop (type_regex, type_regex) type_regex;
    ";", poly_binop (type_regex, type_rltl) type_regex;
    ":", poly_binop (type_regex, type_rltl) type_regex;

    (* Rltl *)
    "not", newty(Tarrow (type_rltl, type_rltl));
    "and", mk_binop (type_rltl, type_rltl) type_rltl;
    "or", mk_binop (type_rltl, type_rltl) type_rltl;
    "implies", mk_binop (type_rltl, type_rltl) type_rltl;
    "iff", mk_binop (type_rltl, type_rltl) type_rltl;
    ";;", mk_binop (type_rltl, type_rltl) type_rltl;
    "::", mk_binop (type_rltl, type_rltl) type_rltl;
    "Cl", newty (Tarrow(type_regex, type_rltl));
  ] in
  List.fold_left (fun env (id,ty) -> add_builtin id ty env) empty_env
    builtin_functions
