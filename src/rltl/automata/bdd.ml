
open Exptree

type t = Cudd.node
type addr = Exptree.node

exception InvalidExpression of addr

type manager =
  { bdd_mgr: Cudd.manager;
    bdd_tbl: (addr, t) Hashtbl.t;
    bdd_vars: (string, t) Hashtbl.t;
  }

let init () =
  {
    bdd_mgr = Cudd.create ();
    bdd_tbl = Hashtbl.create 8;
    bdd_vars = Hashtbl.create 8;
  }

let named_var {bdd_mgr; bdd_vars} s =
  if Hashtbl.mem bdd_vars s then
    Hashtbl.find bdd_vars s
  else
    let x = Cudd.newvar bdd_mgr in
    Hashtbl.add bdd_vars s x;
    x
(*
let trans_bool mgr bddmgr node =
  let {bdd_mgr;bdd_tbl;bdd_vars} = bddmgr in

  let rec tr_bool n =
    (* Check if the node is already translated *)
    if Hashtbl.mem bdd_tbl n then
      Hashtbl.find bdd_tbl n

    (* Look for the node in the expression manager  *)
    else begin
      let e =
        try Manager.lookup mgr n
        with Not_found -> raise (InvalidExpression n)
      in
      match e.Exptree.exp_bool with
      | None -> raise (InvalidExpression n)
      | Some b ->
        begin
          let x = match b with
            | BoolTrue -> Cudd.dtrue bdd_mgr
            | BoolFalse -> Cudd.dfalse bdd_mgr
            | BoolIdent s ->
              if Hashtbl.mem bdd_vars s then
                Hashtbl.find bdd_vars s
              else
                let x = Cudd.newvar bdd_mgr in
                Hashtbl.add bdd_vars s x;
                x
            | BoolNot x -> Cudd.dnot (tr_bool x)
            | BoolOr (x,y) -> Cudd.dor (tr_bool x) (tr_bool y)
            | BoolAnd (x,y) -> Cudd.dand (tr_bool x) (tr_bool y)
          in
          Hashtbl.add bdd_tbl n x;
          x
        end
    end
  in
  tr_bool node
*)
let indexed_vars mgr =
  let vars = Array.make (Hashtbl.length mgr.bdd_vars) "" in
  Hashtbl.iter (fun s n -> vars.(Cudd.index n) <- s) mgr.bdd_vars;
  vars

let manager_apply f mgr = Hashtbl.iter f mgr.bdd_tbl
