(*
#!/usr/bin/env ocaml
#directory "../extlib-1.5";;
#load "extLib.cma"
#load "str.cma"  (* cygwin: replace the ocaml by the result of "ocamlmktop -o ocaml str.cma" *)
*)

open ExtString

let result2html (result:(int*string*string*bool*bool*bool*bool*bool*bool) list) : string =
	let str = ref "" in
	str := !str^"<html>\n";
	str := !str^"<head>\n";
	str := !str^"  <style type=\"text/css\">";
	str := !str^"    body { margin-left: 1in; margin-right: 1in; font-family : sans-serif; font-size : 10pt; }";
	str := !str^"    table {	width: 100%; border: 1px solid Gray; }";
	str := !str^"    th { font-family: sans-serif; font-size: 10pt; background-color: #EEFFF4; font-weight: bold }";
	str := !str^"    td { font-family: sans-serif; font-size: 10pt; background-color: #EEEEFF; }";
	str := !str^"  </style>";
	str := !str^"</head>\n";
	
	str := !str^"<body>\n";
	str := !str^"2GO: 2-way Gastin/Oddoux construction (only for very weak ABAs)<br>";
	str := !str^"2MH: 2-way Miyano/Hayashi construction<br>\n<br>";
	str := !str^"<table>\n";
	str := !str^"  <tr>\n";
	str := !str^"    <th rowspan=\"2\"></th>\n";
	str := !str^"    <th rowspan=\"2\">RTL / LTL</th>\n";
	str := !str^"    <th colspan=\"2\">2GO</th>\n";
	str := !str^"    <th colspan=\"2\">2MH</th>\n";
	str := !str^"  </tr>\n";
	str := !str^"  <tr>\n";
	str := !str^"    <th> &sube; </th>\n";
	str := !str^"    <th> &supe; </th>\n";
	str := !str^"    <th> &sube; </th>\n";
	str := !str^"    <th> &supe; </th>\n";
	str := !str^"  </tr>\n";

	let ok (i:int) (file:string) : string = 
		let link = "<a href=\"l"^(string_of_int i)^"_"^file^"\" style=\"color: green\">OK</a>" in
		"<td rowspan=\"2\" style=\"text-align:center\">"^link in
	let bad (i:int) (file:string) : string  = 
		let link = "<a href=\"l"^(string_of_int i)^"_"^file^"\" style=\"color: red\">BAD</a>" in
		"<td rowspan=\"2\" style=\"text-align:center\">"^link in
	let none = "<td rowspan=\"2\" style=\"text-align:center\">--" in
	
	List.iter 
		begin fun (nr, rtl, ltl, vwsub, vwsup, sub1, sup1, sub2, sup2) ->
			str := !str^"  <tr>\n";
			str := !str^("    <td rowspan=\"2\" style=\"vertical-align:top\">"^(string_of_int nr)^"</td>\n");
			str := !str^("    <td>"^rtl^"</td>\n");
			str := !str^("    "^(if vwsub then (if sub1 then (ok nr "gosub.smv") else (bad nr "gosub.smv")) else none)^"</td>\n");
			str := !str^("    "^(if vwsup then (if sup1 then (ok nr "gosup.smv") else (bad nr "gosup.smv")) else none)^"</td>\n");
			str := !str^("    "^(if sub2 then (ok nr "mhsub.smv") else (bad nr "mhsup.smv"))^"</td>\n");
			str := !str^("    "^(if sup2 then (ok nr "mhsup.smv") else (bad nr "mhsup.smv"))^"</td>\n");
			str := !str^"  </tr>\n";
			str := !str^"  <tr>\n";
			str := !str^("    <td>"^ltl^"</td>\n");
			str := !str^"  </tr>\n"
		end
		(List.rev result);
	!str^"</table>\n</body>\n</table>\n";;





let _ =

	(* parse command line arguments *)
	let mode = ref "" in
	let input = ref "dummy" in
	let rtl2ba = ref "dummy" in
	let nusmv = ref "dummy" in
						
	let usage = "usage: "^Sys.argv.(0)^" <options>\noptions:" in
	let speclist = [
		("-htm", Arg.Tuple [Arg.String (fun s -> mode:="-htm"; input:=s); Arg.Set_string(rtl2ba); Arg.Set_string(nusmv)], "<file.csv> <rtl2ba path> <NuSMV path>: html output of checked formulas");
	] in
	Arg.parse 
		speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;

	match !mode with
		| "-htm" -> 				
				let ic = open_in !input in
				let lines = Std.input_list ic in  
				let lines2 = List.map (fun s -> String.split s ",") lines in
				close_in ic;
				let result = ref [] in  

				if not(Sys.file_exists !rtl2ba) then  
					failwith ("Error: "^(!rtl2ba)^" is not the path to rtl2ba.");
				if not(Sys.file_exists !nusmv) then  
					failwith ("Error: "^(!nusmv)^" is not the path to NuSMV.");
								
				let nr = ref 0 in  (* line number *)
				let check ((rtl:string), (ltl:string)) : unit =
					nr := !nr + 1;
					let snr = string_of_int !nr in
					prerr_endline ltl;  (* progressing status *)
					(* Call rtl2ba and NuSMV *)
					let exe1 = (" "^(!rtl2ba)^" -subeq \""^rtl^"\" \""^ltl^"\" -prefix \"dk_\" > l"^snr^"_gosub.smv") in
					let smv1 = (!nusmv^" -dcx l"^snr^"_gosub.smv > l"^snr^"_gosub.txt") in
					let _ = Sys.command exe1 in
					let _ = Sys.command smv1 in
					let exe2 = (" "^(!rtl2ba)^" -supeq \""^rtl^"\" \""^ltl^"\" -prefix \"dk_\" > l"^snr^"_gosup.smv") in
					let smv2 = (!nusmv^" -dcx l"^snr^"_gosup.smv > l"^snr^"_gosup.txt") in
					let _ = Sys.command exe2 in
					let _ = Sys.command smv2 in

					(* Call rtl2ba with -dgo and NuSMV *)
					let exe1 = (" "^(!rtl2ba)^" -dgo -subeq \""^rtl^"\" \""^ltl^"\" -prefix \"dk_\" > l"^snr^"_mhsub.smv") in
					let smv1 = (!nusmv^" -dcx l"^snr^"_mhsub.smv > l"^snr^"_mhsub.txt") in
					let _ = Sys.command exe1 in
					let _ = Sys.command smv1 in
					let exe2 = (" "^(!rtl2ba)^" -dgo -supeq \""^rtl^"\" \""^ltl^"\" -prefix \"dk_\" > l"^snr^"_mhsup.smv") in
					let smv2 = (!nusmv^" -dcx l"^snr^"_mhsup.smv > l"^snr^"_mhsup.txt") in
					let _ = Sys.command exe2 in
					let _ = Sys.command smv2 in
					
					(* Evaluate the results of translation *)
					let is_veryweak (s:string) : bool = 
						let reg = Str.regexp "-- ABA is very weak" in
						try
							let _ = Str.search_forward reg s 0 in
							true
						with _ -> false
					in
					
					(* Evaluate the results of NuSMV *)
					let is_ok (s:string) : bool =
						let reg = Str.regexp "-- specification.+is true" in
						try
							let _ = Str.search_forward reg s 0 in
							true
						with _ -> false
					in

					(* very weak info *)
					let ic = open_in ("l"^snr^"_gosub.smv") in
					let gosub_smv = Std.input_all ic in
					close_in ic;
					let ic = open_in ("l"^snr^"_gosup.smv") in
					let gosup_smv = Std.input_all ic in
					close_in ic;
					
					(* RTL = LTL *)
					let ic = open_in ("l"^snr^"_gosub.txt") in
					let gosub = Std.input_all ic in
					close_in ic;
					let ic = open_in ("l"^snr^"_gosup.txt") in
					let gosup = Std.input_all ic in
					close_in ic;

					(* RTL = LTL with -dgo *)
					let ic = open_in ("l"^snr^"_mhsub.txt") in
					let mhsub = Std.input_all ic in
					close_in ic;
					let ic = open_in ("l"^snr^"_mhsup.txt") in
					let mhsup = Std.input_all ic in
					close_in ic;
					
					(* save result *)
					result := 
						(!nr, rtl, ltl, 
						is_veryweak gosub_smv, is_veryweak gosup_smv, 
						is_ok gosub, is_ok gosup, 
						is_ok mhsub, is_ok mhsup) :: !result;
				in
				List.iter check	lines2;
				print_endline (result2html !result)
		| _ -> 
			Arg.usage speclist usage


