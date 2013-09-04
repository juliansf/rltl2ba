
open Exptypes
open Exptree

type node = Node.t
type manager = Manager.t

exception Undefined_node of node
exception Node_type_clash of node
exception Non_overlapping_node of node
exception Node_already_overlapped of node

(* Base expression - invalid node *)
let base =
  { exp_bool = None;
    exp_regex = None;
    exp_rltl = None;
  }

(* Build a new expression *)
let _bool x = { base with exp_bool=Some x }
let _regex x = { base with exp_regex=Some x }
let _rltl x = { base with exp_rltl=Some x }

(* Node identification *)
let node_id n = n

(* Initialization - Creates a new manager. *)
let init () =
  let mgr = Manager.init () in
  let const_false =
    { exp_bool = Some BoolFalse;
      exp_regex = Some RegexFalse;
      exp_rltl = Some RltlFalse;
    } in
  let const_true =
    { exp_bool = Some BoolTrue;
      exp_regex = Some RegexTrue;
      exp_rltl = Some RltlTrue;
    } in
  ignore(Manager.add mgr const_false); (* 0 *)
  ignore(Manager.add mgr const_true);  (* 1 *)
  mgr

(* Constants *)
let const_false = 0
let const_true = 1

let getexp mgr node =
  try Manager.lookup mgr node
  with Not_found -> raise (Undefined_node node)

let make_bool mgr node =
  let exp = getexp mgr node in
  match exp.exp_bool with
  | Some _ -> ()
  | None -> raise (Node_type_clash node)

let make_regex mgr node =
  let exp = getexp mgr node in
  match exp.exp_regex with
  | Some _ -> ()
  | None -> make_bool mgr node;
    exp.exp_regex <- Some (RegexProp node)

let make_rltl mgr node =
  let exp = getexp mgr node in
  match exp.exp_rltl with
  | Some _ -> ()
  | None -> make_regex mgr node;
    exp.exp_rltl <- Some(RltlSeq(Existential, WithoutOverlap, node, const_true))

(* Node type tests *)
let is_bool mgr node =
  match (getexp mgr node).exp_bool with
  | Some _ -> true
  | None -> false

let is_regex mgr node =
  match (getexp mgr node).exp_regex with
  | Some _ -> true
  | None -> false

let is_rltl mgr node =
  match (getexp mgr node).exp_rltl with
  | Some _ -> true
  | None -> false

(* New variable with labeling *)
let new_var mgr s =
  Manager.add mgr (_bool (BoolIdent s))

(* Boolean Expressions *)
let bool_not mgr node =
  make_bool mgr node;
  Manager.add mgr (_bool (BoolNot node))

let bool_and mgr n1 n2 =
  make_bool mgr n1;
  make_bool mgr n2;
  Manager.add mgr (_bool (BoolAnd (n1,n2)))

let bool_or mgr n1 n2 =
  make_bool mgr n1;
  make_bool mgr n2;
  Manager.add mgr (_bool (BoolOr (n1,n2)))

let bool_impl mgr n1 n2 =
  let not_n1 = bool_not mgr n1 in
  make_bool mgr n2;
  bool_or mgr not_n1 n2

let bool_iff mgr n1 n2 =
  let not_n1 = bool_not mgr n1 in
  let not_n2 = bool_not mgr n2 in
  let n1_and_n2 = bool_and mgr n1 n2 in
  let not_n1_and_not_n2 = bool_and mgr not_n1 not_n2 in
  bool_or mgr n1_and_n2 not_n1_and_not_n2

(* Regex Expressions *)
let regex_star mgr node =
  make_regex mgr node;
  Manager.add mgr (_regex (RegexStar node))

let regex_plus mgr n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  Manager.add mgr (_regex (RegexPlus (n1, n2)))

let regex_cap mgr n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  Manager.add mgr (_regex (RegexCap (n1, n2)))

let regex_concat mgr ofl n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  Manager.add mgr (_regex (RegexConcat (ofl, n1, n2)))

(* RLTL Expressions *)
let rltl_not mgr node =
  make_rltl mgr node;
  Manager.add mgr (_rltl (RltlNot node))

let rltl_or mgr n1 n2 =
  make_rltl mgr n1;
  make_rltl mgr n2;
  Manager.add mgr (_rltl (RltlOr (n1, n2)))

let rltl_and mgr n1 n2 =
  make_rltl mgr n1;
  make_rltl mgr n2;
  Manager.add mgr (_rltl (RltlAnd (n1, n2)))

let rltl_impl mgr n1 n2 =
  let not_n1 = rltl_not mgr n1 in
  make_rltl mgr n2;
  rltl_or mgr not_n1 n2

let rltl_iff mgr n1 n2 =
  let not_n1 = rltl_not mgr n1 in
  let not_n2 = rltl_not mgr n1 in
  let n1_and_n2 = rltl_and mgr n1 n2 in
  let not_n1_and_not_n2 = rltl_and mgr not_n1 not_n2 in
  rltl_or mgr n1_and_n2 not_n1_and_not_n2

let rltl_seq mgr sfl ofl r x =
  make_regex mgr r;
  make_rltl mgr x;
  Manager.add mgr (_rltl (RltlSeq (sfl, ofl, r, x)))

let rltl_power mgr pfl ofl x y r =
  make_rltl mgr x;
  make_rltl mgr y;
  make_regex mgr r;
  Manager.add mgr (_rltl (RltlPower (pfl, ofl, x, y, r)))

let rltl_closure mgr node =
  make_regex mgr node;
  Manager.add mgr (_rltl (RltlClosure node))

let mk_overlap mgr node =
  (match getexp mgr node with
  | {exp_bool=Some _} -> raise (Node_type_clash node)
  | {exp_bool=None; exp_regex=None; exp_rltl=Some rexp} as e ->
    let rexp' = match rexp with
      | RltlSeq(_,WithOverlap,_,_)
      | RltlPower(_,WithOverlap,_,_,_) -> raise (Node_already_overlapped node)
      | RltlSeq(sfl,WithoutOverlap,r,x) -> RltlSeq(sfl,WithOverlap,r,x)
      | RltlPower(pfl,WithoutOverlap,x,y,r) -> RltlPower(pfl,WithOverlap,x,y,r)
      | _ -> raise (Non_overlapping_node node)
    in
    e.exp_rltl <- Some rexp'

  | {exp_bool=None; exp_regex=Some rexp } as e ->
    let rexp' = match rexp with
      | RegexConcat(WithOverlap,_,_) -> raise (Node_already_overlapped node)
      | RegexConcat(WithoutOverlap,n1,n2) -> RegexConcat(WithOverlap,n1,n2)
      | _ -> raise (Non_overlapping_node node)
    in
    e.exp_regex <- Some rexp'
  | _ -> raise (Non_overlapping_node node));
  node


let overlap = function
  | true -> WithOverlap
  | false -> WithoutOverlap

let existential () = Existential
let universal () = Universal

let regular () = RegularPower
let weak () = WeakPower
let dual () = DualPower
let dualweak () = DualWeakPower
