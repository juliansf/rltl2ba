(** Routines to handle automata in the symbolic encoding of SMV/NuSMV *)
open Sets

(** Positive boolean expression with variables as atoms *) 
type varexp = 
	| True
	| False
	| Var of string            (* boolean variable, e.g., s = TRUE *)
	| NextVar of string
	| Int of string * int      (* e.g. s = 12 *) 
	| NextInt of string * int
	| Neg of varexp
	| And of varexp * varexp
	| Or of varexp * varexp
	| Imp of varexp * varexp
	| Eq of varexp * varexp

(** Type of a SMV module. We use 'varexp list' for pretty formating *)
type smv = {
	vars : string list;                   (* boolean variables *)
	ints : (string * int list) list;      (* for very weak ABA translation *)
	init : varexp list;
	trans : varexp list;
	fair : varexp list;
}


(** Simplifies all the varexp expression in an SMV module *)
let simplify (prog:smv) : smv =
	(* Removes false and true propositions when they are superfluous. *)
	let rec remttff (vexp:varexp)	: varexp = match vexp with
		| True 
		| False 
		| Var _ 
		| NextVar _ 
		| Int _
		| NextInt _ -> vexp
		| Neg e1 -> 
				let ret1 = remttff e1 in
				begin match ret1 with
					| True -> False
					| False -> True
					| _ -> Neg ret1
				end
		| And (e1, e2) -> 
				let ret1 = remttff e1 in
				let ret2 = remttff e2 in
				begin match (ret1, ret2) with
					| (False, _)
					| (_, False) -> False
					| (True, _) -> ret2
					| (_, True) -> ret1
					| _ -> And (ret1, ret2)
				end
		| Or (e1, e2) -> 
				let ret1 = remttff e1 in
				let ret2 = remttff e2 in
				begin match (ret1, ret2) with
					| (True, _) 
					| (_, True) -> True
					| (False, _) -> ret2
					| (_, False) -> ret1
					| _ -> Or (ret1, ret2)
				end
		| Imp (e1, e2) ->
				let ret1 = remttff e1 in
				let ret2 = remttff e2 in
				begin match (ret1, ret2) with
					| (False, _) 
					| (_, True) -> True
					| (True, _) -> ret2
					| (_, False) -> Neg ret1
					| _ -> Imp (ret1, ret2)
				end
		| Eq (e1, e2) ->
				let ret1 = remttff e1 in
				let ret2 = remttff e2 in
				begin match (ret1, ret2) with
					| (True, _) -> ret2  
					| (_, True) -> ret1
					| (False, _) -> Neg ret2
					| (_, False) -> Neg ret1
					| _ -> Eq (ret1, ret2)
				end
	in	
	(* Removes True expressions in a varexp list *)
	let remtrue (list:varexp list) : varexp list =
		List.filter (fun vexp -> not(vexp = True)) list
	in
	let init = remtrue (List.map remttff prog.init) in
	let trans = remtrue (List.map remttff prog.trans) in
	let fair = remtrue (List.map remttff prog.fair) in
	{vars=prog.vars; ints=prog.ints; init=init; trans=trans; fair=fair}


(** Converts a varexp into a string that can be parsed by SMV *)
let rec varexp2str (vexp:varexp) : string = match vexp with
	| True -> "TRUE"
	| False -> "FALSE"
	| Var s -> s
	| NextVar s -> "next("^s^")"
	| Int (s, i) -> "("^s^" = "^(string_of_int i)^")"
	| NextInt (s, i) -> "(next("^s^") = "^(string_of_int i)^")"
	| Neg e -> "!"^(varexp2str e)
	| And (e1, e2) -> "("^(varexp2str e1)^" & "^(varexp2str e2)^")"
	| Or (e1, e2) -> "("^(varexp2str e1)^" | "^(varexp2str e2)^")"
	| Imp (e1, e2) -> "("^(varexp2str e1)^" -> "^(varexp2str e2)^")"
	| Eq (e1, e2) -> "("^(varexp2str e1)^" <-> "^(varexp2str e2)^")"
 

(** Converts a SMV module into a string that can be parsed by SMV *)
let toStr (prog:smv) : string =
	let vars = List.fold_left (fun str v -> str^("  "^v^" : boolean;\n")) "VAR\n" prog.vars in
	let ints = 
		(* Prints an enum declaration *)
		let enumdecl ((s:string), (values:int list)) : string = match values with
			| [] -> "";
			| hd::[] -> "  "^s^" : {"^(string_of_int hd)^"};\n"
			| hd::tl -> 
					let set =	List.fold_left
						(fun str v -> str^", "^(string_of_int v))
						(string_of_int hd)
						tl
					in
					"  "^s^" : {"^set^"};\n"
		in
		List.fold_left
			(fun str tup -> str^(enumdecl tup))
			""
			prog.ints
	in
	let init = List.fold_left (fun str bvs -> str^("INIT\n  "^(varexp2str bvs)^"\n")) "" prog.init in
	let trans = List.fold_left (fun str bvs -> str^("TRANS\n  "^(varexp2str bvs)^"\n")) "" prog.trans in
	let fair = List.fold_left (fun str bvs -> str^("FAIRNESS\n  "^(varexp2str bvs)^"\n")) "" prog.fair in
	vars^ints^init^trans^fair
	
	
	
	
	

(** Converts a SMV module into a SPIN neverclaim *)
let print_neverclaim (prog:smv) : unit =
	(* THIS TRANSLATION ONLY WORKS IF STATES ARE BOOLEAN VARS *)
	(* This translation works only for one fairness condition *)
	if List.length prog.fair > 1 then 
		failwith "smv.ml: SMV module must encode a Büchi automaton (just one FAIRNESS constraint).";
	
	(* Make big conjunction of a list of varexp formulas *)
	let mergelist (lst:varexp list) : varexp = match lst with 
		| [] -> True
		| h::[] -> h
		| h::tl -> List.fold_left (fun exp tr -> And (exp, tr)) h tl 
	in  

	(* List of all variables that occur in alltrans expression *)
	let props : string list = 
		let rec vars (e:varexp) : string list = match e with
			| True -> []
			| False -> []
			| Var s -> if not(List.mem s prog.vars) then [s] else []
			| NextVar s -> if not(List.mem s prog.vars) then [s] else []
			| Int _ -> failwith "smv.ml: only boolean variables allowed" 
			| NextInt _ -> failwith "smv.ml: only boolean variables allowed"
			| Neg exp1 -> vars exp1
			| And (exp1, exp2)
			| Or (exp1, exp2)
			| Imp (exp1, exp2) 				
			| Eq (exp1, exp2) -> List.rev_append (vars exp1) (vars exp2)
		in
		let props = Sets.fromList (vars (mergelist prog.trans)) in
		List.rev props
	in
	
(* Example BDD from Wikipedia
	let allvars = ["x1"; "x2"; "x3"] in
	let x1 = Var "x1" in
	let x2 = Var "x2" in
	let x3 = Var "x3" in
	let nx1 = Neg x1 in
	let nx2 = Neg x2 in
	let nx3 = Neg x3 in
	let m1 = And (nx1, And (nx2, nx3)) in
	let m2 = And (x2, x3) in
	let m3 = And (x1, x2) in 
	let alltrans = Or (m1, Or (m2, m3)) in
*)
	
		
			
	(* Map variables to 0..2(|vars|-1) on every other position *)
	(* 'Next variables' are directed to odd positions *)
	(* TODO: reorder variables to r0 s0 r1 s1 ... *)
	let ((var2i:int StrMap.t), _) = 
		List.fold_left 
			(fun (map, i) s -> (StrMap.add s i map, i+2))
			(StrMap.empty, 0)
			(props @ prog.vars)     (* <----- VARIABLE ORDER *)
	in

	(* Replaces all vars x by next(x) *)
	let rec toNext (exp:varexp) : varexp = match exp with
			| True -> True
			| False -> False
			| Var s -> NextVar s
			| NextVar s -> failwith "smv.ml: NextVar should not occur"
			| Int _ -> failwith "smv.ml: only boolean variables allowed" 
			| NextInt _ -> failwith "smv.ml: only boolean variables allowed"
			| Neg exp1 -> Neg (toNext exp1) 
			| And (exp1, exp2) -> And (toNext exp1, toNext exp2)
			| Or (exp1, exp2) -> Or (toNext exp1, toNext exp2)
			| Imp (exp1, exp2) -> Imp (toNext exp1, toNext exp2) 
			| Eq (exp1, exp2) -> Eq (toNext exp1, toNext exp2)
		in

	(* Build BDD representation from transition formula *)
	let makebdd (exp:varexp) (man:Cudd.manager) (vars:Cudd.node array) : Cudd.node = 
		let isAtom (e:varexp) : bool = match e with
			| True | False | Var _ | NextVar _ -> true
			| _ -> false
		in
		let rec mkbdd (exp:varexp) = match exp with
			| True -> Cudd._ReadOne man
			| False -> Cudd._ReadLogicZero man
			| Var s -> let i = StrMap.find s var2i in	vars.(i)
			| NextVar s -> let i = StrMap.find s var2i in	vars.(i+1)
			| Int _ -> failwith "smv.ml: only boolean variables allowed" 
			| NextInt _ -> failwith "smv.ml: only boolean variables allowed"
			| Neg exp1 -> 
					let n = mkbdd exp1 in
					let not_n = Cudd._Not n in
					Cudd._Ref not_n;
					if not(isAtom exp1) then Cudd._RecursiveDeref man n;
					not_n
			| And (exp1, exp2) ->
					let n1 = mkbdd exp1 in
					let n2 = mkbdd exp2 in
					let n1_and_n2 = Cudd._bddAnd man n1 n2 in
					Cudd._Ref n1_and_n2;
					if not(isAtom exp1) then Cudd._RecursiveDeref man n1;
					if not(isAtom exp2) then Cudd._RecursiveDeref man n2;
					n1_and_n2
			| Or (exp1, exp2) ->
					let n1 = mkbdd exp1 in
					let n2 = mkbdd exp2 in
					let n1_or_n2 = Cudd._bddOr man n1 n2 in
					Cudd._Ref n1_or_n2;
					if not(isAtom exp1) then Cudd._RecursiveDeref man n1;
					if not(isAtom exp2) then Cudd._RecursiveDeref man n2;
					n1_or_n2
			| Imp (exp1, exp2) -> 
					mkbdd (Or (Neg exp1, exp2))
			| Eq (exp1, exp2) -> 
					mkbdd (And (Imp (exp1, exp2), Imp (exp2, exp1)))
		in
		mkbdd exp
	in

(*
	StrMap.iter (fun k v -> print_endline (k^" -> "^(string_of_int v))) var2i;
	let rec vars e = match e with
		| True -> ()
		| False -> ()
		| Var s -> print_string (s^" ")
		| NextVar s -> print_string (s^" ")
		| Int _ -> failwith "smv.ml: only boolean variables allowed" 
		| NextInt _ -> failwith "smv.ml: only boolean variables allowed"
		| Neg exp1 -> vars exp1
		| And (exp1, exp2)
		| Or (exp1, exp2)
		| Imp (exp1, exp2) 				
		| Eq (exp1, exp2) -> vars exp1; vars exp2
	in
	vars alltrans;
*)	
	(* Converts a cube into a monomial as string *)
	let print_cube (arr:int array) (label:string) (isInit:bool) : unit =
		(* Using the isInit flag is a hack. If set, the automaton *)
		(* may choose the right state *) 
		
		let now = ref "" in
		let next = ref "" in
		let i2var = Array.of_list (props @ prog.vars) in  (* maps an integer to the variable *)
		(* Returns true if the i-th index maps to a proposition *)
		let maps2prop (i:int) : bool = (i/2 < List.length props) in  
		Array.iteri
			(fun i v -> 
				(*(match v with
					| 0 -> print_string "0"
					| 1 -> print_string "1"
					| _ -> print_string "-");*)
				match i mod 2 with
					| 0 ->  (* normal variables *)
						begin match v with 
							| 0 -> 
									if isInit then begin
										if maps2prop i then 
											now := !now^("!("^i2var.(i/2)^") & ")
									end else begin
										now := !now^("!("^i2var.(i/2)^") & ")
									end
							| 1 -> 
									if isInit then begin
										if (maps2prop i) then 
											now := !now^("("^i2var.(i/2)^") & ")
									end else begin
										now := !now^("("^i2var.(i/2)^") & ")
									end
							| _ -> ()
						end
					| _ ->  (* next variables *)
						begin match v with 
							| 0 -> 
									if maps2prop i then 
										next := !next^("!("^i2var.(i/2)^"); ") 
									else 
										next := !next^(""^i2var.(i/2)^"=0; ")
							| 1 -> 
									if maps2prop i then 
										next := !next^("("^i2var.(i/2)^"); ")
									else 
										next := !next^(""^i2var.(i/2)^"=1; ")
							| _ -> 
									if maps2prop i	then 
										()
									else
										next := !next^("if :: "^i2var.(i/2)^"=0 :: "^i2var.(i/2)^"=1 fi; ")
						end
			)
			arr;
		(*print_endline "";*)
		now := !now^"1";
		if !next = "" then next := "skip;";
		print_endline ("  :: "^(!now)^" -> atomic{"^(!next)^"goto "^label^"};");		
	in


	let numVars = 2*(List.length (prog.vars @ props)) in  (* x and next(x), for all x *) 
	let man = Cudd._Init () in
	let vars = Array.init numVars (fun i -> Cudd._bddNewVar man) in
	
	List.iter
		(fun v -> print_endline ("bool "^v^";"))
		prog.vars;
	print_endline "never {";
	(* Transitions from the initial states *)
	let exp = And (mergelist prog.trans, mergelist prog.init) in
	let bdd = makebdd exp man vars in
	print_endline "start:";
	print_endline "  if";
	let pr (arr:int array) : unit = print_cube arr "accepting" true in
	Cudd._ForeachPrime pr man bdd;
	print_endline "  fi;";
	Cudd._RecursiveDeref man bdd;
	
	(* Transitions from accepting states *)
	print_endline "accepting:";
	print_endline "  if";
	(* -- Transitions to accepting states *)
	let exp = And (mergelist prog.trans, mergelist prog.fair) in
	let bdd = makebdd exp man vars in
	let pr (arr:int array) : unit = print_cube arr "accepting" false in
	Cudd._ForeachPrime pr man bdd;
	Cudd._RecursiveDeref man bdd;
	(* -- Transitions to nonaccepting states *)
	let exp = And (mergelist prog.trans, Neg (mergelist prog.fair)) in
	let bdd = makebdd exp man vars in
	let pr (arr:int array) : unit = print_cube arr "nonaccepting" false in
	Cudd._ForeachPrime pr man bdd;
	Cudd._RecursiveDeref man bdd;
	print_endline "  fi;";

	(* Transitions from nonaccepting states *)
	print_endline "nonaccepting:";
	print_endline "  if";
	(* -- Transitions to accepting states *)
	let exp = And (mergelist prog.trans, mergelist prog.fair) in
	let bdd = makebdd exp man vars in
	let pr (arr:int array) : unit = print_cube arr "accepting" false in
	Cudd._ForeachPrime pr man bdd;
	Cudd._RecursiveDeref man bdd;
	(* -- Transitions to nonaccepting states *)
	let exp = And (mergelist prog.trans, Neg (mergelist prog.fair)) in
	let bdd = makebdd exp man vars in
	let pr (arr:int array) : unit = print_cube arr "nonaccepting" false in
	Cudd._ForeachPrime pr man bdd;
	Cudd._RecursiveDeref man bdd;
	print_endline "  fi;";
	print_endline "}";

	
(*			
	print_endline (Std.dump allvars);
	print_endline (Std.dump numVars);
	print_endline (Std.dump (Cudd._DagSize bdd));
	StrMap.iter (fun x y -> print_endline ((Std.dump x)^" -> "^(Std.dump y))) var2i;
	(* Print minterms *)
	let count = ref 0 in
	(*Cudd._ForeachPrime print_cube man bdd;*)
	
	(*Cudd._PrintMinterm man bdd;*)
*)

	Cudd._RecursiveDeref man bdd;
	Cudd._Quit(man)	
	
		
				
			