type 'a bool_exp =
	| True
	| False
	| Atom of 'a
	| Neg of 'a bool_exp
	| And of 'a bool_exp * 'a bool_exp
	| Or of 'a bool_exp * 'a bool_exp;;

type bexp = string bool_exp;;

type sere =
	| BExp of bexp
	| Star of sere
	| Col of sere * sere
	| Conc of sere * sere
	| Cup of sere * sere
	| Cap of sere * sere;;

type psl =
	| Prop of string
	| NProp of string  (* negated prop used in negated normal form *)
	| OpNeg of psl
	| OpAnd of psl * psl
	| OpOr of psl * psl
	| OpX of psl
	| OpY of psl
	| OpZ of psl
	| OpF of psl
	| OpG of psl
	| OpO of psl
	| OpH of psl
	| OpU of psl * psl
	| OpR of psl * psl
	| OpS of psl * psl
	| OpT of psl * psl
	| Fby of sere * psl
	| Trig of sere * psl
	| BFby of sere * psl
	| BTrig of sere * psl
	| Cl of sere
	| NCl of sere;;


module Sere = struct
	type t = sere
	let compare = compare
end
module SereSet = Set.Make(Sere);;
module SereMap = Map.Make(Sere);;


(** 'Time property' of given PSL formula *)
type timeprop = FutureFormula | PastFormula | MixedFormula 



(** Checks whether a boolean expression models a proposition *)
let rec is_model (exp:bexp) (s:string) : bool = match exp with
	True -> true
	| False -> false
	| Atom p -> if s=p then true else false
	| Neg exp1 -> not(is_model exp1 s)
	| And (exp1, exp2) -> (is_model exp1 s) || (is_model exp2 s)
	| Or (exp1, exp2) -> (is_model exp1 s) && (is_model exp2 s);;



(** Translates a PSL formula into negative normal form *)
let rec toNnf (phi:psl) : psl = match phi with
	| Prop s -> Prop s
	| NProp s -> Prop s
	| OpAnd (psi1, psi2) -> OpAnd (toNnf psi1, toNnf psi2)
	| OpOr (psi1, psi2) -> OpOr (toNnf psi1, toNnf psi2)
	| OpNeg psi ->
			begin match psi with
				| Prop s -> NProp s
				| NProp s -> Prop s
				| OpNeg psi1 ->  toNnf psi1
				| OpAnd (psi1, psi2) -> OpOr (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| OpOr (psi1, psi2) -> OpAnd (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| OpX psi1 -> OpX (toNnf (OpNeg psi1))
				| OpY psi1 -> OpZ (toNnf (OpNeg psi1))
				| OpZ psi1 -> OpY (toNnf (OpNeg psi1))
				| OpF psi1 -> OpG (toNnf (OpNeg psi1))
				| OpG psi1 -> OpF (toNnf (OpNeg psi1))
				| OpO psi1 -> OpH (toNnf (OpNeg psi1))
				| OpH psi1 -> OpO (toNnf (OpNeg psi1))
				| OpU (psi1, psi2) -> OpR (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| OpR (psi1, psi2) -> OpU (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| OpS (psi1, psi2) -> OpT (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| OpT (psi1, psi2) -> OpS (toNnf (OpNeg psi1), toNnf (OpNeg psi2))
				| Fby (reg, psi1) -> Trig (reg, toNnf (OpNeg psi1))
				| Trig (reg, psi1) -> Fby (reg, toNnf (OpNeg psi1))
				| BFby (reg, psi1) -> BTrig (reg, toNnf (OpNeg psi1))
				| BTrig (reg, psi1) -> BFby (reg, toNnf (OpNeg psi1))
				| Cl reg -> NCl reg
				| NCl reg -> Cl reg
			end
	| OpX psi1 -> OpX (toNnf psi1)
	| OpY psi1 -> OpY (toNnf psi1)
	| OpZ psi1 -> OpZ (toNnf psi1)
	| OpF psi1 -> OpF (toNnf psi1)
	| OpG psi1 -> OpG (toNnf psi1)
	| OpO psi1 -> OpO (toNnf psi1)
	| OpH psi1 -> OpH (toNnf psi1)
	| OpU (psi1, psi2) -> OpU (toNnf psi1, toNnf psi2)
	| OpR (psi1, psi2) -> OpR (toNnf psi1, toNnf psi2)
	| OpS (psi1, psi2) -> OpS (toNnf psi1, toNnf psi2)
	| OpT (psi1, psi2) -> OpT (toNnf psi1, toNnf psi2)
	| Fby (reg, psi1) -> Fby (reg, toNnf psi1)
	| Trig (reg, psi1) -> Trig (reg, toNnf psi1)
	| BFby (reg, psi1) -> BFby (reg, toNnf psi1)
	| BTrig (reg, psi1) -> BTrig (reg, toNnf psi1)
	| Cl reg -> Cl reg
	| NCl reg -> Cl reg


(** Converts a boolean expression into a string *)
let rec bexp2str (b:bexp) : string = match b with
	| True 
	| False -> failwith "Psl: no true/false in sere."
	| Atom s -> s
	| Neg c -> "!"^(bexp2str c)
	| And (c, d) -> "("^(bexp2str c)^" & "^(bexp2str d)^")"
	| Or (c, d) -> "("^(bexp2str c)^" | "^(bexp2str d)^")";;

(** Converts a SERE into a string *)
let rec sere2str (r:sere) : string = match r with
	| BExp b -> bexp2str b
	| Star r1 -> "("^(sere2str r1)^")*"
	| Col (r1, r2) -> "("^(sere2str r1)^": "^(sere2str r2)^")"
	| Conc (r1, r2) -> "("^(sere2str r1)^"; "^(sere2str r2)^")"
	| Cup (r1, r2) -> "("^(sere2str r1)^" || "^(sere2str r2)^")"
	| Cap (r1, r2) -> "("^(sere2str r1)^" && "^(sere2str r2)^")";;

(** converts a PSL formula into a string in nuSMV syntax. *)
let rec toStr (phi:psl) : string = match phi with 
	| Prop s -> s
	| NProp s -> "!("^s^")"
	| OpNeg psi -> "!("^(toStr psi)^")"
	| OpAnd (psi1,psi2) -> "("^(toStr psi1)^" & "^(toStr psi2)^")"
	| OpOr (psi1,psi2) -> "("^(toStr psi1)^" | "^(toStr psi2)^")"
	| OpX psi -> "X ("^(toStr psi)^")"
	| OpY psi -> "Y ("^(toStr psi)^")"
	| OpZ psi -> "Z ("^(toStr psi)^")"
	| OpF psi1 -> "F ("^(toStr psi1)^")"
	| OpG psi1 -> "G ("^(toStr psi1)^")"
	| OpO psi1 -> "O ("^(toStr psi1)^")"
	| OpH psi1 -> "H ("^(toStr psi1)^")"
	| OpU (psi1,psi2) -> "("^(toStr psi1)^" U "^(toStr psi2)^")"
	| OpR (psi1,psi2) -> "("^(toStr psi1)^" V "^(toStr psi2)^")"
	| OpS (psi1,psi2) -> "("^(toStr psi1)^" S "^(toStr psi2)^")"
	| OpT (psi1,psi2) -> "("^(toStr psi1)^" T "^(toStr psi2)^")"
	| Fby (r,psi) -> "({"^(sere2str r)^"} <>-> "^(toStr psi)^")"
	| Trig (r,psi) -> "({"^(sere2str r)^"} []-> "^(toStr psi)^")"
	| BFby (r,psi) -> "({"^(sere2str r)^"} <->-> "^(toStr psi)^")"
	| BTrig (r,psi) -> "({"^(sere2str r)^"} [-]-> "^(toStr psi)^")"
	| Cl re -> "Cl("^(sere2str re)^")"
	| NCl re -> "! Cl("^(sere2str re)^")"


module Phi = struct
	type t = psl
	let compare = compare
end
module PhiSet = Set.Make(Phi);;
module PhiMap = Map.Make(Phi);;


(** Retrieves all subformulas of a PSL formula. *)
let rec getSub (phi:psl) : PhiSet.t = match phi with
	| Cl _ 
	| NCl _ 
	| Prop _	
	| NProp _ -> PhiSet.singleton phi
	| OpNeg psi	
	| OpX psi	
	| OpY psi	
	| OpZ psi	
	| OpF psi
	| OpG psi
	| OpO psi
	| OpH psi
	| Fby (_,psi)	
	| Trig (_,psi) 
	| BFby (_,psi)	
	| BTrig (_,psi) -> PhiSet.add phi (getSub psi)
	| OpAnd (psi1,psi2)	
	| OpOr (psi1,psi2) 
	| OpU (psi1,psi2) 
	| OpR (psi1,psi2) 
	| OpS (psi1,psi2) 
	| OpT (psi1,psi2) -> PhiSet.union (getSub psi1) (getSub psi2)


(** Returns a set of all used propositions of the PSL formula. *)
let getProps (phi:psl) : string list =
	let rec traverse_bexp (exp:bexp) : string list = match exp with
		| True 
		| False -> []
		| Atom s -> [s]
		| Neg ex1 -> traverse_bexp ex1
		| And (ex1, ex2) 
		| Or (ex1, ex2) -> (traverse_bexp ex1) @ (traverse_bexp ex2)
	in
	let rec traverse_re (re:sere) : string list = match re with
		| BExp ex1 -> traverse_bexp ex1 
		| Star re1 -> traverse_re re1
		| Col (re1, re2)
		| Conc (re1, re2)
		| Cup (re1, re2)
		| Cap (re1, re2) -> (traverse_re re1) @ (traverse_re re2)
	in	
	let rec traverse_psl (psi:psl) : string list = match psi with
		| Cl re1 
		| NCl re1 -> traverse_re re1 
		| Prop s	
		| NProp s -> [s]
		| OpNeg psi1	
		| OpX psi1	
		| OpY psi1	
		| OpZ psi1	
		| OpF psi1
		| OpG psi1
		| OpO psi1
		| OpH psi1	-> traverse_psl psi1
		| Fby (re, psi1)	
		| Trig (re, psi1) 
		| BFby (re, psi1)	
		| BTrig (re, psi1) -> (traverse_re re) @ (traverse_psl psi1)
		| OpAnd (psi1, psi2)	
		| OpOr (psi1, psi2) 
		| OpU (psi1, psi2) 
		| OpR (psi1, psi2) 
		| OpS (psi1, psi2) 
		| OpT (psi1, psi2) -> (traverse_psl psi1) @ (traverse_psl psi2)
	in
	(* Removes duplicates from a list and hence, returns a set *)
	let rem_duplicates (lst:'a list) : 'a list =
		let sorted_lst = List.sort compare lst in
		List.fold_left
			(fun newlst el -> 
				if newlst!=[] && (List.hd newlst)=el then newlst else el::newlst) 
			[]
			sorted_lst
	in
	rem_duplicates (traverse_psl phi)

(** Checks whether the formula is a past formula. *)
let rec isPast (phi:psl) : bool = match phi with
	| Cl _ | NCl _ | Prop _	| NProp _ -> false
	| OpY _	| OpZ _ | OpO _	| OpH _ | BFby _ | BTrig _ | OpS _ | OpT _ -> true
	| OpNeg psi	| OpX psi | OpF psi | OpG psi | Fby (_,psi) | Trig (_,psi) -> isPast psi 
	| OpAnd (psi1, psi2)
	| OpOr (psi1, psi2) 
	| OpU (psi1, psi2) 
	| OpR (psi1, psi2) -> (isPast psi1) || (isPast psi1)

	
	
	
(** Checks whether the formula is a past formula. *)
let rec isFuture (phi:psl) : bool = match phi with
	| Cl _ | NCl _ | Prop _	| NProp _ -> false
	| OpX _	| OpF _ | OpG _| Fby _ | Trig _ | OpU _ | OpR _ -> true
	| OpNeg psi	| OpY psi | OpZ psi | OpO psi	| OpH psi | BFby (_,psi) | BTrig (_,psi) -> isPast psi 
	| OpAnd (psi1, psi2)
	| OpOr (psi1, psi2) 
	| OpS (psi1, psi2) 
	| OpT (psi1, psi2) -> (isPast psi1) || (isPast psi1)


(** Calculates the 'time property' of a psl formula. *)
let timeprop (phi:psl) : timeprop =
	let p = isPast phi in
	let f = isFuture phi in
	if p && f then MixedFormula else
		if f then FutureFormula else
			PastFormula


(** Returns a NuSMV header with propositions as variables *)
let smvheader (phi:psl) : string =
	let str = ref "" in
	str := !str^"MODULE main\n";
	str := !str^"VAR\n";
	List.iter
		(fun s -> match s with
			| "0"
			| "1"
			| "TRUE"
			| "FALSE" -> ()
			| _ ->str := !str^"  "^s^" : boolean;\n")
		(getProps phi);
	!str^"\n"

