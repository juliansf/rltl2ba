(* Representation of types and declarations *)

open Asttypes

type type_expr =
  { mutable typ_desc: type_desc;
    typ_id: int;
  }

and type_desc =
| Tarrow of type_expr * type_expr
| Tvar of string option
| Tpoly of type_expr * type_expr * (type_expr -> type_expr)
