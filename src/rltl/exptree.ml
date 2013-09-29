
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
| RltlNot of node
| RltlOr of node * node
| RltlAnd of node * node
| RltlSeq of seq_flag * overlap_flag * node * node
| RltlPower of power_flag * overlap_flag * node * node * node
| RltlClosure of node


(* Auxiliary functions *)
(* Base expression - invalid node *)
let base =
  { exp_bool = None;
    exp_regex = None;
    exp_rltl = None;
  }

let equal_expr x y =
  match x.exp_bool, y.exp_bool with
  | Some x, Some y -> x=y
  | None, None ->
    begin
      match x.exp_regex, y.exp_regex with
      | Some x, Some y -> x=y
      | None, None ->
        begin
          match x.exp_rltl, y.exp_rltl with
          | Some x, Some y when x=y -> true
          | None, None -> true
          | _ -> false
        end
      | _ -> false
    end
  | _ -> false
