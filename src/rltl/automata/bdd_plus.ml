
type t = Cudd.node
type minmod = True | False | Mod of int list list

type manager =
  { plus_mgr: Cudd.manager; }

let init () =
  {
    plus_mgr = Cudd.create ();
  }

let dtrue mgr = Cudd.dtrue mgr.plus_mgr
let dfalse mgr = Cudd.dfalse mgr.plus_mgr
let ithvar mgr = Cudd.ithvar mgr.plus_mgr
let newvar_at_level mgr = Cudd.newvar_at_level mgr.plus_mgr
let index = Cudd.index
let is_true = Cudd.is_true
let is_false = Cudd.is_false
let dor = Cudd.dor
let dand = Cudd.dand

let min_models node =
  if Cudd.is_true node then True
  else if Cudd.is_false node then False
  else
  let cubes = Cudd.bdd_cubes node in
  let keep_positive (i,l) = function
    | Cudd.True -> (i+1), (i::l)
    | _ -> (i+1), l
  in
  let clean term =
    let (_,l) = Array.fold_left keep_positive (0,[]) term in
    List.rev l
  in
  Mod (List.map clean cubes)

let map mgr f node =
  if Cudd.is_true node || Cudd.is_false node then node
  else begin
    let bddmgr = mgr.plus_mgr in
    let n = Cudd.size bddmgr in
    let cubes = Cudd.bdd_cubes node in
    let _and x (i,no) = (i-1),
      match x with
      | Cudd.True ->
        if f i < 0 then Cudd.dfalse bddmgr
        else (Cudd.dand (Cudd.ithvar bddmgr (f i)) no)
      | _ -> no
    in
    let _or bdd term =
      let (_,no) = Array.fold_right _and term (n-1, Cudd.dtrue bddmgr) in
      Cudd.dor bdd no
    in
    List.fold_left _or (Cudd.dfalse bddmgr) cubes
  end

let shift_by mgr k node = map mgr (fun i -> i+k) node
