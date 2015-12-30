
open Entry
open Typedtree
open Location
open Rltl

type entry = Entry.t

let rec free_vars exp =
  let env = Hashtbl.create 8 in
  let free = Hashtbl.create 8 in
  let rec fvars e =
    match e.texp_desc with
    | Texp_boolconst _ -> ()
    | Texp_ident id ->
      if not (Hashtbl.mem env id)
      then begin
        try
          let ocurr = Hashtbl.find free id in
          incr ocurr
        with Not_found -> Hashtbl.add free id (ref 1)
      end
    | Texp_apply (f, el) -> fvars f; List.iter fvars el
    | Texp_power (_,x,y,None) -> List.iter fvars [x;y]
    | Texp_power (_,x,y,Some r) -> List.iter fvars [x;y;r]
    | Texp_overlap e -> fvars e
    | Texp_closure e -> fvars e
    | Texp_let ((v,e1),e2) ->
      begin
        let (f,args) = trans_var v in
        List.iter (fun x -> Hashtbl.add env x ()) args;
        fvars e1;
        List.iter (fun x -> Hashtbl.remove env x) args;
        Hashtbl.add env f ();
        fvars e2;
        Hashtbl.remove env f
      end
  in
  fvars exp;
  let fl = Hashtbl.fold (fun x n fl -> (x,!n)::fl) free [] in
  List.fast_sort (fun (s1,n1) (s2,n2) ->
    if n1=n2 then compare s1 s2 else n2-n1) fl


and trans_var var =
  match var.var_desc with
  | Tvar_ident id -> (id.txt, [])
  | Tvar_funct (f, args) -> (f.txt, List.map (fun (x,_) -> x.txt) args)

let rec trans_expr (mgr: Expgen.manager) renv (e: Typedtree.expression) : entry =
  match e.texp_desc with
  | Texp_boolconst "true" -> ValEntry (Expgen.const_true)
  | Texp_boolconst "false" -> ValEntry (Expgen.const_false)
  | Texp_boolconst s ->
    Misc.fatal_error ("Translate.Texp_boolconst: Invalid value "^s)

  | Texp_ident sid ->
    begin
      try Hashtbl.find renv sid
      with Not_found ->
        Misc.fatal_error ("Translate.trans_expr.Texp_ident: " ^ sid)
    end

  | Texp_apply (sfunct, sargs) ->
    let rec apply (fentry: entry) : Typedtree.expression list -> entry = function
      | [] -> fentry
      | arg::args ->
        let f = try fun_entry fentry
          with _ -> begin
            Hashtbl.iter (fun x e -> Format.printf "(%s,%s) "
              x (match e with ValEntry _ -> "ValEntry" | _ -> "FunEntry")) renv;
            Misc.fatal_error "Translate.trans_expr.Texp_apply"
          end in
        let argentry = trans_expr mgr renv arg in
        apply (f argentry) args
    in
    let functentry : entry = trans_expr mgr renv sfunct in
    apply functentry sargs

  | Texp_power (pflag, sx, sy, sr) ->
    let xnode = val_entry (trans_expr mgr renv sx) in
    let ynode = val_entry (trans_expr mgr renv sy) in
    let rnode = match sr with
      | Some r -> val_entry (trans_expr mgr renv r)
      | None -> Expgen.const_true
    in
    let pfl = match pflag with
      | Asttypes.Until -> Expgen.regular()
      | Asttypes.WeakUntil -> Expgen.weak()
      | Asttypes.Release -> Expgen.dual()
      | Asttypes.StrongRelease -> Expgen.dualweak()
    in
    let ofl = Expgen.overlap false in (*XXX We need a way to modify this *)
    ValEntry (Expgen.rltl_power mgr pfl ofl xnode ynode rnode)

  | Texp_overlap sexp ->
    let expnode = val_entry (trans_expr mgr renv sexp) in
    let onode =
      try Expgen.mk_overlap mgr expnode
      with
        | Expgen.Node_type_clash _ ->
          Misc.fatal_error "Translate.trans_exp.Texp_overlap"
        | Expgen.Non_overlapping_node _ (*->*)
       (* raise (Error (Non_overlapping_expression, sexp.texp_loc))*)
        | Expgen.Node_already_overlapped _ ->
          Location.prerr_warning e.texp_loc
            (Warnings.Generic "this overlap does nothing.");
          expnode
    in
    ValEntry onode

  | Texp_closure sexp ->
    let expnode = val_entry (trans_expr mgr renv sexp) in
    ValEntry(Expgen.rltl_closure mgr expnode)

  | Texp_let ((v, def), body) ->
    let fname, args = trans_var v in
    let rec f = function
      | [] -> let defentry = trans_expr mgr renv def in
              List.iter (Hashtbl.remove renv) args;
              defentry
      | x::xs -> FunEntry (fun arg -> Hashtbl.add renv x arg; f xs)
    in
    let fentry = f args in
    Hashtbl.add renv fname fentry;
    let bodyentry = trans_expr mgr renv body in
    Hashtbl.remove renv fname;
    bodyentry

let expression expected_type typed_exp =
  let ty = typed_exp.texp_type in
  let loc = typed_exp.texp_loc in

  (* Check that type of [typed_exp] is subtype of expected type *)
  if not (Btype.subtype ty expected_type) then
    raise (Typecheck.Error
             (Typecheck.Expression_type_clash (ty,expected_type), loc));

  (* Create the expresion manager *)
  let manager = Expgen.init () in

  (* Create the table of references *)
  let renv : (Ident.t, entry) Hashtbl.t = Hashtbl.create 8 in

  (* Add the predefined operators *)
  Operpredef.build_initial_env (Hashtbl.add renv) manager;

  (* Compute free vars (the alphabet) *)
  let fv = free_vars typed_exp in
  (*XXX List.iter (fun (x,n) -> Format.printf "(%s,%d) " x n) fv;*)

  (* Add the free vars to the environment of references *)
  List.iter (fun (v,_) ->
    if not (Hashtbl.mem renv v) then
      Hashtbl.add renv v (ValEntry (Expgen.new_var manager v))) fv;

  (* Translate Expression *)
  let entry =
    try
      trans_expr manager renv typed_exp
    with e ->
      Rltl.Expgen.print_manager Format.err_formatter manager;
      raise e
  in
  (* Return the manager and the computed node *)
  manager, match entry with
  | ValEntry v -> Expgen.link manager v; Expgen.clean manager; v
  | FunEntry _ -> Misc.fatal_error "Translate.match entry"


let print_expr ppf (mgr, node) =
  Rltl.Expgen.print_manager ppf mgr;
  Format.fprintf ppf "\n# Result: %d\n" (Rltl.Expgen.node_id node)

let automata (mgr,node) =
  try
  let autmgr = Automata.init mgr in
  let automata =
    if !Clflags.nfa then
      Automata.Nfa (Automata.get_nfa autmgr node)
    else if !Clflags.nbw then
      let rank = match !Clflags.ranking with
        | "full" -> Automata.fullRank
        | "max2" -> Automata.maxTwoRank
        | _ -> Automata.stratifiedRank
      in
      Automata.Nbw (Automata.get_nbw ~rank:rank autmgr node)
    else if !Clflags.ahw then
      Automata.Ahw (Automata.get_ahw autmgr node ~simpl:false)
    else
      Automata.Ahw (Automata.get_ahw autmgr node ~simpl:false)
  in
  autmgr,automata
  with Not_found ->
    let s = Format.sprintf "Translate.automata(%d,%B,%B,%B)"
      (Rltl.Expgen.node_id node) !Clflags.nfa !Clflags.ahw !Clflags.nbw in
    failwith s

let print_automata fmt (mgr,automata) =
  (*Automata.print_manager fmt mgr;*)
  match automata with
  | Automata.Nfa nfa ->
    begin
      if !Clflags.dot then
        Format.fprintf fmt "/* NFA: */@\n%a"
          (Automata.nfa2dot mgr) nfa
      else
        Format.fprintf fmt "-- NFA:@\n%a"
          (Automata.print_nfa mgr) nfa
    end
  | Automata.Ahw ahw ->
    begin
      if !Clflags.dot then
        Format.fprintf fmt "/* AHW: */@\n%a"
          (Automata.ahw2dot mgr) ahw
      else
        Format.fprintf fmt "-- AHW:@\n%a"
          (Automata.print_ahw mgr) ahw
    end
  | Automata.Nbw nbw ->
    begin
      if !Clflags.dot then
        Format.fprintf fmt "/* NBW: */@\n%a"
          (Automata.nbw2dot mgr) nbw
      else
        Format.fprintf fmt "/* NBW */@\n%a"
          (Automata.print_nbw mgr) nbw
    end
