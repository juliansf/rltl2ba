
open Exptree
open Exptypes
open Shared

exception InvalidExpression of node
exception ExpressionNotFound of node

module Nbw = Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B)))
module Ahw = Ahw.Make(Nfa.Make(Bool.Default.B))
module Nfa = Nfa.Make(Bool.Default.B)
module Cudd = Bool.Default.B

type t =
  {
    mutable aut_size: int;
    aut_bddmgr: Bdd.manager;
    aut_expmgr: Manager.t;
    aut_ahwmgr: Ahw.manager;
    aut_nbwmgr: Nbw.manager;
    aut_label: (Node.t, Cudd.t) Hashtbl.t;
    aut_nfa: (Node.t, Nfa.t) Hashtbl.t;
    aut_ahw: (Node.t, Ahw.t) Hashtbl.t;
    aut_nbw: (Node.t, Nbw.t) Hashtbl.t;
  }

type label = Cudd.t
type nfa = Nfa.t
type ahw = Ahw.t
type nbw = Nbw.t

type automata =
| Nbw of nbw
| Ahw of ahw
| Nfa of nfa

let fullRank = Shared.FullRank
let maxTwoRank = Shared.MaxTwoRank
let stratifiedRank = Shared.StratifiedRank

let init expmgr =
  let size = Manager.size expmgr in
  let bddmgr = Bdd.init () in
  let ahwmgr = Ahw.init bddmgr in
  {
    aut_size = size;
    aut_bddmgr = bddmgr;
    aut_expmgr = expmgr;
    aut_ahwmgr = ahwmgr;
    aut_nbwmgr = Nbw.init ahwmgr;
    aut_label = Hashtbl.create size;
    aut_nfa = Hashtbl.create size;
    aut_ahw = Hashtbl.create size;
    aut_nbw = Hashtbl.create size;
  }

let size {aut_expmgr} = Manager.size aut_expmgr

(** Translate a bool expression into BDD label, if needed *)
let rec get_label mgr node =
  (* Check if node is already translated *)
  if Hashtbl.mem mgr.aut_label node then
    Hashtbl.find mgr.aut_label node

  (* Look for the node in the expression manager *)
  else begin
    (*let {Bdd.bdd_mgr;bdd_tbl;bdd_vars} = mgr.aut_bddmgr in*)
    let e =
      try Manager.lookup mgr.aut_expmgr node
      with Not_found -> raise (ExpressionNotFound node)
    in
    Expgen_private.make_bool mgr.aut_expmgr node;
    match e.Exptree.exp_bool with
    | None -> raise (InvalidExpression node)
    | Some b ->
      begin
        let x = match b with
          | BoolTrue -> Cudd.dtrue
          | BoolFalse -> Cudd.dfalse
          | BoolIdent s -> Cudd.datom s
          | BoolNot x -> Cudd.dnot (get_label mgr x)
          | BoolOr (x,y) -> Cudd.dor (get_label mgr x) (get_label mgr y)
          | BoolAnd (x,y) -> Cudd.dand (get_label mgr x) (get_label mgr y)
        in
        Hashtbl.add mgr.aut_label node x;
        x
      end
  end

(** Translate a regex expression into an NFA, if needed *)
let rec get_nfa mgr node =
  (*Printf.fprintf stderr "Getting NFA for node %d... %!" node;*)
  (* Check if node is already translated *)
  if Hashtbl.mem mgr.aut_nfa node then begin
    (*Printf.fprintf stderr "done (cached)\n%!";*)
    Hashtbl.find mgr.aut_nfa node
  end

  (* Look for the node in expression manager and translate it. *)
  else begin
    let e =
      try Manager.lookup mgr.aut_expmgr node
      with Not_found -> raise (ExpressionNotFound node)
    in
    Expgen_private.make_regex mgr.aut_expmgr node;
    match e.Exptree.exp_regex with
    | None -> raise (InvalidExpression node)
    | Some r ->
      begin
        let x = match r with
          | RegexFalse -> Nfa.nfa_false
          | RegexTrue -> Nfa.letter (get_label mgr node)
          | RegexProp n -> Nfa.letter (get_label mgr n)
          | RegexStar n -> Nfa.star (get_nfa mgr n)
          | RegexPlus (n1,n2) -> Nfa.plus (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexCap (n1,n2) -> Nfa.product (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexConcat (WithOverlap,n1,n2) ->
            Nfa.fusion (get_nfa mgr n1) (get_nfa mgr n2)
          | RegexConcat (WithoutOverlap,n1,n2) ->
            Nfa.concat (get_nfa mgr n1) (get_nfa mgr n2)
        in
        Hashtbl.add mgr.aut_nfa node x;
        (*Format.fprintf Format.err_formatter "%a\n"
          (Printnfa.print_nfa
          (fun fmt l -> Format.fprintf fmt "%s" (Bool.Default.B.to_string l)))*)
        x
      end;
  end

(** Translate an rltl expression into an AHW, if needed *)
let rec get_ahw ?simpl:(simpl=false) mgr node =
  (* Check if node is already translated *)
  if Hashtbl.mem mgr.aut_ahw node then
    Hashtbl.find mgr.aut_ahw node

  (* Look for the node in the expression manager and translate it. *)
  else begin
    let amgr = mgr.aut_ahwmgr in
    let e =
      try Manager.lookup mgr.aut_expmgr node
      with Not_found -> raise (ExpressionNotFound node)
    in
    Expgen_private.make_rltl mgr.aut_expmgr node;
    match e.Exptree.exp_rltl with
    | None -> raise (InvalidExpression node)
    | Some r -> try
      begin
        let rho = match r with
          | RltlTrue -> Ahw.top amgr
          | RltlFalse -> Ahw.bottom amgr
          | RltlProp n -> Ahw.letter amgr (get_label mgr n)
          | RltlNot n -> failwith "__file__:__line__: [internal error] cannot ocurr."
            (*Ahw.negate (get_ahw mgr n)*)
          | RltlOr (n1,n2) ->
            let a1 = get_ahw mgr n1 and a2 = get_ahw mgr n2 in
            let a = Ahw.disj amgr a1 a2 in
            (*Expgen_private.unlink mgr.aut_expmgr n1;
            if (Expgen_private.links mgr.aut_expmgr n1 <= 0) then
              Ahw.unlink_ref amgr a1;
            Expgen_private.unlink mgr.aut_expmgr n2;
            if (Expgen_private.links mgr.aut_expmgr  n2 <= 0) then
              Ahw.unlink_ref amgr a2;*)
            a
          | RltlAnd (n1,n2) ->
            (try Ahw.conj amgr (get_ahw mgr n1) (get_ahw mgr n2)
            with Not_found -> failwith "Autmanager.get_ahw.RltlAnd")
          | RltlSeq (kind, olap, nr, nx) -> (try
            begin
              let r = get_nfa mgr nr in
              let x = get_ahw mgr nx in
              let f = match kind, olap with
                | Existential, WithOverlap -> Ahw.fusion
                | Existential, WithoutOverlap -> Ahw.concat
                | Universal, WithOverlap -> Ahw.univ_fusion
                | Universal, WithoutOverlap -> Ahw.univ_concat
              in
              try f amgr r x with Not_found -> failwith "RltlSeq.f"
            end with Not_found -> failwith "__file__:__line__: RltlSeq")
          | RltlPower (kind, olap, nx, ny, nr) -> (try
            begin
              let r = get_nfa mgr nr in
              let x = get_ahw mgr nx in
              let y = get_ahw mgr ny in
              let f = match kind, olap with
                | RegularPower, WithOverlap -> Ahw.power_fusion
                | RegularPower, WithoutOverlap -> Ahw.power
                | WeakPower, WithOverlap -> Ahw.weak_power_fusion
                | WeakPower, WithoutOverlap -> Ahw.weak_power
                | DualPower, WithOverlap -> Ahw.dual_power_fusion
                | DualPower, WithoutOverlap -> Ahw.dual_power
                | DualWeakPower, WithOverlap -> Ahw.dual_weak_power_fusion
                | DualWeakPower, WithoutOverlap -> Ahw.dual_weak_power
              in
              f amgr x r y
            end with Not_found -> failwith "__file__:__line__: RltlPower")
          | RltlClosure (cfl, n) ->
            begin
              match cfl with
              | Positive -> Ahw.closure amgr (get_nfa mgr n)
              | Negative -> Ahw.dual_closure amgr (get_nfa mgr n)
            end
        in
        (*if simpl then Ahw.simplify amgr rho;*)
        Logger.debug ~level:10 "Adding %d -> %d\n" node rho;
        Hashtbl.add mgr.aut_ahw node rho;
        Ahw.link_ref mgr.aut_ahwmgr rho;
        rho
      end
      with Not_found -> failwith "__file__:__line__: get_ahw"
  end

let get_ahw ?simpl:(simpl=false) mgr node =
  let ahw = get_ahw ~simpl mgr node in
  Hashtbl.iter (fun n r ->
      if n != node then begin
        Ahw.unlink_ref mgr.aut_ahwmgr r;
        Hashtbl.remove mgr.aut_ahw n
      end
    ) mgr.aut_ahw;
  Ahw.clean mgr.aut_ahwmgr;
  ahw

let get_nbw ?(rank=Shared.StratifiedRank) mgr node =
  let ahw = get_ahw ~simpl:true mgr node in
  Nbw.from_ahw ~rank:rank mgr.aut_nbwmgr ahw



(*XXX Add cache functions that can track the number of links *)
