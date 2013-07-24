let _ =
	let succ = Array.of_list [[1; 1]; [1; 2; 3; 2; 3; 1; 1]; [1; 1; 1]; []; []] in
	let _ = Graph.sccs succ in
	
	(* parse command line arguments *)
	let mode = ref "" in
	let input = ref "dummy" in
	let input2 = ref "dummy" in
	let prefix = ref "" in
	let old = ref false in
	let simp = ref true in
	let dgo = ref false in
	
	let usage = "usage: "^Sys.argv.(0)^" [options]\noptions:" in
	let speclist = [
		("-phi", Arg.String (fun s -> mode:="-phi"; input:= s), "<RTL> : outputs the formula");
		("-smv", Arg.String (fun s -> mode:="-smv"; input:= s), "<RTL> : outputs a NuSMV module");
		("-spin", Arg.String (fun s -> mode:="-spin"; input:= s), "<RTL> : outputs a SPIN neverclaim");
		("-aba-", Arg.String (fun s -> mode:="-aba-"; input:= s), "<RTL> : outputs the ABA without optimization");
		("-aba", Arg.String (fun s -> mode:="-aba"; input:= s), "<RTL> : outputs the optimized ABA");
		("-nfa", Arg.String (fun s -> mode:="-nfa"; input:= s), "<SERE> : outputs the NFA");
		("-mnfa", Arg.String (fun s -> mode:="-mnfa"; input:= s), "<SERE> : outputs the mirrored NFA");
		("-vars", Arg.String (fun s -> mode:="-vars"; input:= s), "<RTL> : outputs the variable declarations in NuSMV");
		("-subeq", Arg.Tuple [Arg.String (fun s -> mode:="-subeq"; input:= s); Arg.Set_string(input2)], "<RTL> <LTL> : outputs a NuSMV module for L(RTL) <= L(LTL)");
		("-supeq", Arg.Tuple [Arg.String (fun s -> mode:="-supeq"; input:= s); Arg.Set_string(input2)], "<RTL> <LTL> : outputs a NuSMV module for L(RTL) >= L(LTL)");
		
		
		("-dgo", Arg.Set(dgo), " : disables Gastin/Oddoux construction");
		("-nos", Arg.Clear(simp), " : disables simplification of transitions");
		("-old", Arg.Set(old), " : old version of translation is used");
		("-prefix", Arg.Set_string(prefix), "<prefix> : to distinguish the output");
		] in
	Arg.parse
		speclist
		(fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
		usage;
	
	(* Parses the given input string and handles errors *)
	let parse (s: string) : Psl.psl =
		let lexbuf = Lexing.from_string s in
		try
			Parser.main Lexer.token lexbuf
		with Parsing.Parse_error ->
				prerr_endline "Syntax error:";
				prerr_endline s;
				for i = 1 to Lexing.lexeme_start lexbuf do
					prerr_string " "
				done;
				prerr_endline "^";
				exit 1
	in
	
	match !mode with
	| "-phi" ->
			let phi = parse !input in
			print_endline (Psl.toStr phi);
	| "-smv" ->
			let phi = parse !input in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			let aba' = if !simp then Aba.simplify aba "TRUE" "FALSE" else aba in
			let smv = Aba.toSmv aba' !prefix !dgo in
			let smv' = if !simp then Smv.simplify smv else smv in
			print_endline ("MODULE neverclaim");
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("-- ABA is "^(if Aba.isVeryWeak aba' then "very weak" else "not very weak"));
			print_endline (Smv.toStr smv');
	(* print_endline (Aba.toSmv2 aba' "TRUE" "FALSE" !prefix); *)
	| "-spin" ->
			let phi = parse !input in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			let aba' = if !simp then Aba.simplify aba "TRUE" "FALSE" else aba in
			dgo := true;  (* disable Gastin/Oddoux *)
			let smv = Aba.toSmv aba' !prefix !dgo in
(*			let smv = Aba.toSmv2 aba' !prefix in *)
			let smv' = if !simp then Smv.simplify smv else smv in
			print_endline ("/* Formula: "^Psl.toStr phi);
			print_endline ("   NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("   ABA is "^(if Aba.isVeryWeak aba' then "very weak" else "not very weak")^" */");
			
			print_endline (Aba.toStr aba "TRUE" "FALSE");
			print_endline (Smv.toStr smv');
			print_endline ("/* "^(Psl.toStr phi)^" */");
			Smv.print_neverclaim smv';
	| "-aba-" ->
			let phi = parse !input in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("-- ABA is "^(if Aba.isVeryWeak aba then "very weak" else "not very weak"));
			print_endline (Aba.toStr aba "TRUE" "FALSE")
	| "-aba" ->
			let phi = parse !input in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			let aba' = if !simp then Aba.simplify aba "TRUE" "FALSE" else aba in
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("-- ABA is "^(if Aba.isVeryWeak aba' then "very weak" else "not very weak"));
			print_endline (Aba.toStr aba' "TRUE" "FALSE")
	| "-nfa" ->
			let phi = parse ("Cl{"^(!input)^"}") in
			let re = match phi with
				| Psl.Cl r -> r
				| _ -> failwith "main: cannot parse sere"
			in
			let nfa = Nfa.fromSere re in
			print_endline ("-- SERE: "^Psl.sere2str re);
			print_endline (Nfa.toStr nfa)
	| "-mnfa" ->
			let phi = parse ("Cl{"^(!input)^"}") in
			let re = match phi with
				| Psl.Cl r -> r
				| _ -> failwith "main: cannot parse sere"
			in
			let nfa = Nfa.mirror (Nfa.fromSere re) in
			print_endline ("-- SERE: "^Psl.sere2str re);
			print_endline (Nfa.toStr nfa)
	| "-vars" ->
			let phi = parse !input in
			let smv = Psl.smvheader phi in
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline smv
	| "-subeq" ->
			let phi = parse !input in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			let aba' = if !simp then Aba.simplify aba "TRUE" "FALSE" else aba in
			let smv = Aba.toSmv aba' !prefix !dgo in
			let smv' = if !simp then Smv.simplify smv else smv in
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("-- ABA is "^(if Aba.isVeryWeak aba then "very weak" else "not very weak"));
			if !dgo then print_endline ("-- 2-way Gastin/Oddoux construction disabled");
			print_endline ((Psl.smvheader phi)^"LTLSPEC\n  "^(!input2)^"\n\n"^(Smv.toStr smv'))
	| "-supeq" ->
			let phi = parse ("!("^(!input)^")") in
			let pslaba = if !old then Pslaba.fromPslOld phi else Pslaba.fromPsl phi "TRUE" "FALSE" in
			let aba = Pslaba.toAba pslaba in
			let aba' = if !simp then Aba.simplify aba "TRUE" "FALSE" else aba in
			let smv = Aba.toSmv aba' !prefix !dgo in
			let smv' = if !simp then Smv.simplify smv else smv in
			print_endline ("-- Formula: "^Psl.toStr phi);
			print_endline ("-- NNF: "^Psl.toStr (Psl.toNnf phi));
			print_endline ("-- ABA is "^(if Aba.isVeryWeak aba then "very weak" else "not very weak"));
			if !dgo then print_endline ("-- 2-way Gastin/Oddoux construction disabled");
			print_endline ((Psl.smvheader phi)^"LTLSPEC\n  !("^(!input2)^")\n\n"^(Smv.toStr smv'))
	| _ ->
			Arg.usage speclist usage


