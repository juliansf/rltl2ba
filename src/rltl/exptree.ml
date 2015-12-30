
open Exptypes

type node = Node.t

and expression =
  { mutable exp_bool: bool_expr option;
    mutable exp_regex: regex_expr option;
    mutable exp_rltl: rltl_expr option;
    mutable exp_links: int; }

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
| RltlClosure of closure_flag * node


(* Auxiliary functions *)
(* Base expression - invalid node *)
let base =
  { exp_bool = None;
    exp_regex = None;
    exp_rltl = None;
    exp_links = 0;
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

let link x =
  x.exp_links <- x.exp_links+1

let unlink x =
  x.exp_links <- x.exp_links-1

let links x =
  x.exp_links

let rec iter_nodes f e =
  begin
    match e.exp_bool with
    | None -> ()
    | Some e' -> match e' with
      | BoolNot x -> f x
      | BoolOr (x,y) -> f x; f y
      | BoolAnd (x,y) -> f x; f y
      | _ -> ()
  end;
  begin
    match e.exp_regex with
    | None -> ()
    | Some e' -> match e' with
      | RegexProp x -> f x
      | RegexStar x -> f x
      | RegexPlus (x,y) -> f x; f y
      | RegexCap (x,y) -> f x; f y
      | RegexConcat (_,x,y) -> f x; f y
      | _ -> ()
  end;
  begin
    match e.exp_rltl with
    | None -> ()
    | Some e' -> match e' with
      | RltlProp x -> f x
      | RltlNot x -> f x
      | RltlOr (x,y) -> f x; f y
      | RltlAnd (x,y) -> f x; f y
      | RltlSeq (_,_,x,y) -> f x; f y
      | RltlPower (_,_,x,r,y) -> f x; f r; f y
      | RltlClosure (_,x) -> f x
      | _ -> ()
  end
