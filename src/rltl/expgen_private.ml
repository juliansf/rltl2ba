
open Exptypes
open Exptree

type node = Node.t
type manager = Manager.t

exception Undefined_node of node
exception Node_type_clash of node
exception Non_overlapping_node of node
exception Node_already_overlapped of node

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

(* Auxiliary functions *)
let getexp mgr node =
  try Manager.lookup mgr node
  with Not_found -> raise (Undefined_node node)

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

(* Node type cast *)
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
  | None ->
    make_regex mgr node;
    exp.exp_rltl <-
      match exp.exp_regex with
      | Some (RegexProp n) -> Some (RltlProp n)
      | _ ->
        Some(RltlSeq(Existential, WithoutOverlap, node, const_true))

let make_idempotent_commutative f x y =
  if x = y then x
  else if x < y then f x y else f y x

(* New variable with labeling *)
let new_var mgr s =
  let node = Manager.add mgr (_bool (BoolIdent s)) in
  Manager.add mgr (_bool (BoolNot node));
  node

(* Boolean Expressions *)
let rec bool_not mgr node =
  make_bool mgr node;
  if node = const_false then const_true
  else if node = const_true then const_false
  else begin
    match (getexp mgr node).exp_bool with
    | None -> failwith "__file__:__line__: [internal error] cannot ocurr."
    | Some bexp -> match bexp with
      | BoolTrue -> const_false
      | BoolFalse -> const_true
      | BoolIdent _ -> Manager.add mgr (_bool (BoolNot node))
      | BoolNot x -> x
      | BoolOr (x,y) ->
        let not_x = bool_not mgr x in
        let not_y = bool_not mgr y in
        Manager.add mgr (_bool (BoolAnd (not_x, not_y)))
      | BoolAnd (x,y) ->
        let not_x = bool_not mgr x in
        let not_y = bool_not mgr y in
        Manager.add mgr (_bool (BoolOr (not_x, not_y)))
  end

let bool_and mgr n1 n2 =
  make_bool mgr n1;
  make_bool mgr n2;
  let f x y =
    if x = const_false || y = const_false then const_false
    else if x = const_true then y
    else if y = const_true then x
    else Manager.add mgr (_bool (BoolAnd(x,y))) in
  make_idempotent_commutative f n1 n2

let bool_or mgr n1 n2 =
  make_bool mgr n1;
  make_bool mgr n2;
  let f x y =
    if x = const_true || y = const_true then const_true
    else if x = const_false then y
    else if y = const_false then x
    else Manager.add mgr (_bool (BoolOr (x,y))) in
  make_idempotent_commutative f n1 n2

let bool_xor mgr n1 n2 =
  if n1 = n2 then const_false
  else if n1 = const_false then n2
  else if n1 = const_true then bool_not mgr n2
  else if n2 = const_false then n1
  else if n2 = const_true then bool_not mgr n1
  else begin
    let not_n1 = bool_not mgr n1 in
    let not_n2 = bool_not mgr n2 in
    let n1_and_not_n2 = bool_and mgr n1 not_n2 in
    let not_n1_and_n2 = bool_and mgr not_n1 n2 in
    bool_or mgr n1_and_not_n2 not_n1_and_n2
  end

let bool_impl mgr n1 n2 =
  if n1 = const_false || n1 = n2 then const_true
  else begin
    let not_n1 = bool_not mgr n1 in
    make_bool mgr n2;
    bool_or mgr not_n1 n2
  end

let bool_iff mgr n1 n2 =
  if n1 = n2 then const_true
  else begin
    let not_n1 = bool_not mgr n1 in
    let not_n2 = bool_not mgr n2 in
    let n1_and_n2 = bool_and mgr n1 n2 in
    let not_n1_and_not_n2 = bool_and mgr not_n1 not_n2 in
    bool_or mgr n1_and_n2 not_n1_and_not_n2
  end

(* Regex Expressions *)
let regex_star mgr node =
  make_regex mgr node;
  Manager.add mgr (_regex (RegexStar node))

let regex_plus mgr n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  let f x y =
    if is_bool mgr x && is_bool mgr y
    then begin
      let b = bool_or mgr x y in
      make_regex mgr b;
      b
    end
    else
      Manager.add mgr (_regex (RegexPlus (x, y))) in
  make_idempotent_commutative f n1 n2

let regex_cap mgr n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  let f x y =
    if is_bool mgr x && is_bool mgr y
    then begin
      let b = bool_and mgr x y in
      make_regex mgr b;
      b
    end
    else
      Manager.add mgr (_regex (RegexCap (x, y))) in
  make_idempotent_commutative f n1 n2

let regex_concat mgr ofl n1 n2 =
  make_regex mgr n1;
  make_regex mgr n2;
  Manager.add mgr (_regex (RegexConcat (ofl, n1, n2)))

(* RLTL Expressions *)
let rec rltl_not mgr node =
  make_rltl mgr node;
  if node = const_false then const_true
  else if node = const_true then const_false
  else begin
    match (getexp mgr node).exp_rltl with
    | None -> failwith "__file__:__line__: [internal error] cannot ocurr."
    | Some rexp ->
      begin match rexp with
      | RltlTrue -> const_false
      | RltlFalse -> const_true
      | RltlProp x ->
        let bnode = bool_not mgr x in
        make_rltl mgr bnode;
        bnode
      | RltlNot _ -> failwith "__file__:__line__: [internal error] cannot ocurr."
      | RltlOr (x,y) ->
        let not_x = rltl_not mgr x in
        let not_y = rltl_not mgr y in
        let f a b = Manager.add mgr (_rltl (RltlAnd (a, b))) in
        make_idempotent_commutative f not_x not_y
      | RltlAnd (x,y) ->
        let not_x = rltl_not mgr x in
        let not_y = rltl_not mgr y in
        let f a b = Manager.add mgr (_rltl (RltlOr (a, b))) in
        make_idempotent_commutative f not_x not_y
      | RltlSeq (sfl, ofl, r, x) ->
        let not_x = rltl_not mgr x in
        let not_sfl = match sfl with
          | Existential -> Universal
          | Universal -> Existential in
        Manager.add mgr (_rltl (RltlSeq (not_sfl, ofl, r, not_x)))
      | RltlPower (pfl, ofl, x, y, r) ->
        let not_x = rltl_not mgr x in
        let not_y = rltl_not mgr y in
        let not_pfl = match pfl with
          | RegularPower -> DualPower
          | DualPower -> RegularPower
          | WeakPower -> DualWeakPower
          | DualWeakPower -> WeakPower in
        Manager.add mgr (_rltl (RltlPower (not_pfl, ofl, not_x, not_y, r)))
      | RltlClosure (cfl, r) ->
        let not_cfl = match cfl with
          | Positive -> Negative
          | Negative -> Positive in
        Manager.add mgr (_rltl (RltlClosure (not_cfl, r)))
      end
  end

let rltl_or mgr n1 n2 =
  make_rltl mgr n1;
  make_rltl mgr n2;
  let f x y = Manager.add mgr (_rltl (RltlOr (x, y))) in
  make_idempotent_commutative f n1 n2

let rltl_and mgr n1 n2 =
  make_rltl mgr n1;
  make_rltl mgr n2;
  let f x y = Manager.add mgr (_rltl (RltlAnd (x, y))) in
  make_idempotent_commutative f n1 n2

let rltl_xor mgr n1 n2 =
  if n1 = n2 then const_false
  else begin
    let not_n1 = rltl_not mgr n1 in
    let not_n2 = rltl_not mgr n2 in
    let n1_and_not_n2 = rltl_and mgr n1 not_n2 in
    let not_n1_and_n2 = rltl_and mgr not_n1 n2 in
    rltl_or mgr n1_and_not_n2 not_n1_and_n2
  end

let rltl_impl mgr n1 n2 =
  let not_n1 = rltl_not mgr n1 in
  make_rltl mgr n2;
  rltl_or mgr not_n1 n2

let rltl_iff mgr n1 n2 =
  if n1 = n2 then const_true
  else begin
    let not_n1 = rltl_not mgr n1 in
    let not_n2 = rltl_not mgr n2 in
    let n1_and_n2 = rltl_and mgr n1 n2 in
    let not_n1_and_not_n2 = rltl_and mgr not_n1 not_n2 in
    rltl_or mgr n1_and_n2 not_n1_and_not_n2
  end

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
  Manager.add mgr (_rltl (RltlClosure (Positive, node)))

let mk_overlap mgr node =
  match getexp mgr node with
  | {exp_bool=Some _} -> raise (Node_type_clash node)
  | {exp_bool=None; exp_regex=None; exp_rltl=Some rexp} as e ->
    let rexp' = match rexp with
      | RltlSeq(_,WithOverlap,_,_)
      | RltlPower(_,WithOverlap,_,_,_) -> raise (Node_already_overlapped node)
      | RltlSeq(sfl,WithoutOverlap,r,x) -> RltlSeq(sfl,WithOverlap,r,x)
      | RltlPower(pfl,WithoutOverlap,x,y,r) -> RltlPower(pfl,WithOverlap,x,y,r)
      | _ -> raise (Non_overlapping_node node)
    in
    (*e.exp_rltl <- Some rexp'*)
    Manager.add mgr (_rltl rexp')

  | {exp_bool=None; exp_regex=Some rexp } as e ->
    let rexp' = match rexp with
      | RegexConcat(WithOverlap,_,_) -> raise (Node_already_overlapped node)
      | RegexConcat(WithoutOverlap,n1,n2) -> RegexConcat(WithOverlap,n1,n2)
      | _ -> raise (Non_overlapping_node node)
    in
    (*e.exp_regex <- Some rexp'*)
    Manager.add mgr (_regex rexp')
  | _ -> raise (Non_overlapping_node node)


let overlap = function
  | true -> WithOverlap
  | false -> WithoutOverlap

let existential () = Existential
let universal () = Universal

let regular () = RegularPower
let weak () = WeakPower
let dual () = DualPower
let dualweak () = DualWeakPower

let positive () = Positive
let negative () = Negative
