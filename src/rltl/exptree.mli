
open Exptypes

type node = Node.t

and expression =
  { mutable exp_bool: bool_expr option;
    mutable exp_regex: regex_expr option;
    mutable exp_rltl: rltl_expr option }

and bool_expr =
| BoolTrue
| BoolFalse
| BoolIdent of string
| BoolNot of node
| BoolOr of node * node
| BoolAnd of node * node

and regex_expr =
| RegexTrue
| RegexFalse
| RegexProp of node
| RegexStar of node
| RegexPlus of node * node
| RegexCap of node * node
| RegexConcat of overlap_flag * node * node

and rltl_expr =
| RltlTrue
| RltlFalse
| RltlProp of node
| RltlNot of node
| RltlOr of node * node
| RltlAnd of node * node
| RltlSeq of seq_flag * overlap_flag * node * node
| RltlPower of power_flag * overlap_flag * node * node * node
| RltlClosure of node


(* Auxiliary functions *)
val equal_expr: expression -> expression -> bool
val base: expression (* invalid node *)
