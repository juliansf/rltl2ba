
open Exptree
(*
type t =
  {
    mutable aut_bddmgr: Bdd.manager;
    aut_label: (Node.t, bool_expr * Bdd.t option ref) Hashtbl.t;
    aut_nfa: (Node.t, regex_expr * Nfa.t option ref) Hashtbl.t;
    aut_abw: (Node.t, rltl_expr * Abw.t option ref) Hashtbl.t;
  }

let make size =
  { aut_bddmgr = Bdd.init ();
    aut_label = Hashtbl.create size;
    aut_nfa = Hashtbl.create size;
    aut_abw = Hashtbl.create size;
  }

let init expmgr =
  let size = Manager.size expmgr in
  let find_exp n =
    Manager.lookup expmgr n in
  let exps = Array.init size find_exp in
  let refs = Array.make size 0 in
  let mgr = make size in

  for i=0 to size-1 do
    (match exps.(i).exp_bool with
    | None -> ()
    | Some e ->
      let n = match e with
        | BoolIdent s ->
          ref (Some (Bdd.named_var mgr.aut_bddmgr s))
        | _ -> ref None
      in
      Hashtbl.add mgr.aut_label i (e, n));
    (match exps.(i).exp_regex with
    | None -> ()
    | Some e -> Hashtbl.add mgr.aut_nfa i (e, ref None));
    (match exps.(i).exp_rltl with
    | None -> ()
    | Some e -> Hashtbl.add mgr.aut_abw i (e, ref None))
  done;
  mgr
*)
(* ************************************************************** *)
exception InvalidExpression of node
exception ExpressionNotFound of node

type t =
  {
    mutable aut_size: int;
    aut_bddmgr: Bdd.manager;
    aut_expmgr: Manager.t;
    aut_label: (Node.t, Bdd.t) Hashtbl.t;
    aut_nfa: (Node.t, Nfa.t) Hashtbl.t;
    aut_abw: (Node.t, Abw.t) Hashtbl.t;
  }

type label = Bdd.t
type nfa = Nfa.t

let init expmgr =
  let size = Manager.size expmgr in
  {
    aut_size = size;
    aut_bddmgr = Bdd.init ();
    aut_expmgr = expmgr;
    aut_label = Hashtbl.create size;
    aut_nfa = Hashtbl.create size;
    aut_abw = Hashtbl.create size;
  }

let size {aut_expmgr} = Manager.size aut_expmgr

(** Translate a bool expression into BDD label, if needed *)
let rec get_label mgr node =
  (* Check if node is already translated *)
  if Hashtbl.mem mgr.aut_label node then
    Hashtbl.find mgr.aut_label node

  (* Look for the node in the expression manager *)
  else begin
    let {Bdd.bdd_mgr;bdd_tbl;bdd_vars} = mgr.aut_bddmgr in
    let e =
      try Manager.lookup mgr.aut_expmgr node
      with Not_found -> raise (ExpressionNotFound node)
    in
    match e.Exptree.exp_bool with
    | None -> raise (InvalidExpression node)
    | Some b ->
      begin
        let x = match b with
          | BoolTrue -> Cudd.dtrue bdd_mgr
          | BoolFalse -> Cudd.dfalse bdd_mgr
          | BoolIdent s ->
            if Hashtbl.mem bdd_vars s then
              Hashtbl.find bdd_vars s
            else begin
              let x = Cudd.newvar bdd_mgr in
              Hashtbl.add bdd_vars s x;
              x
            end
          | BoolNot x -> Cudd.dnot (get_label mgr x)
          | BoolOr (x,y) -> Cudd.dor (get_label mgr x) (get_label mgr y)
          | BoolAnd (x,y) -> Cudd.dand (get_label mgr x) (get_label mgr y)
        in
        Hashtbl.add bdd_tbl node x;
        x
      end
  end

(** Translate a regex expression into an NFA, if needed *)
let rec get_nfa mgr node =
  (* Check if node is already translated *)
  if Hashtbl.mem mgr.aut_nfa node then
    Hashtbl.find mgr.aut_nfa node

  (* Look for the node in expression manager and translate it. *)
  else begin
    let e =
      try Manager.lookup mgr.aut_expmgr node
      with Not_found -> raise (ExpressionNotFound node)
    in
    match e.Exptree.exp_regex with
    | None -> raise (InvalidExpression node)
    | Some r ->
      begin
        let x = match r with
          | RegexFalse -> Nfa.nfa_false
          | RegexTrue -> Nfa.letter (get_label mgr node)
          | RegexProp n -> Nfa.letter (get_label mgr n)
          | RegexStar n -> Nfa.star (get_nfa mgr n)
          | RegexPlus (n1,n2) ->
            Nfa.plus (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexCap (n1,n2) -> Nfa.product (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexConcat (Exptypes.WithOverlap,n1,n2) ->
            Nfa.fusion (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexConcat (Exptypes.WithoutOverlap,n1,n2) ->
            Nfa.concat (get_nfa mgr n1) (get_nfa mgr n2)
        in
        Hashtbl.add mgr.aut_nfa node x;
        x
      end
  end
