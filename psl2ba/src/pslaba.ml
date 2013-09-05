(** Routines for alternating Büchi automata (ABAs) that are obtained form 
		RTL formulas. *)

(*
   An RTLABA consists of
      states Q that are idenified with subformulas of a given RTL formula,
      an alphabet S=2^P \cup {true, false}, where P is the set of propositions,
      a transition function: Q --> \calB(P \cup \negP \cup Q)}, and
      an acceptance set F \subseteq Q.
*)

(* abbreviations *)
type psl = Psl.psl
type nfa = Nfa.nfa

type aba_state =
	| Prop of string
	| NProp of string  (* negated prop used in negated normal form *)
	| OpAnd of aba_state * aba_state
	| OpOr of aba_state * aba_state
	| OpX of aba_state
	| OpY of aba_state
	| OpZ of aba_state
	| OpF of aba_state
	| OpG of aba_state
	| OpO of aba_state
	| OpH of aba_state
	| OpU of aba_state * aba_state
	| OpS of aba_state * aba_state
	| OpR of aba_state * aba_state
	| OpT of aba_state * aba_state
	| Fby of int * Psl.sere * aba_state  (* state of NFA given by sere and aba_state *)
	| BFby of int * Psl.sere * aba_state
	| Trig of int * Psl.sere * aba_state
	| BTrig of int * Psl.sere * aba_state
	| Cl of int * Psl.sere
	| NCl of int * Psl.sere;;

module AbaSt = struct
	type t = aba_state
	let compare = compare
end
module AbaStSet = Set.Make(AbaSt);;
module AbaStMap = Map.Make(AbaSt);;


(** Defines the type of an RTLABA. The transition function is defined as 
		delta: states --> \calB(Propositions \cup (states \times direction)) *)
type dir = Back | WBack | Eps | Forw  (* directions: back, weak back, epsilon, or forward transition *)
type succ =  Pr of string | NPr of string | St of aba_state * dir
type succ_states = succ Psl.bool_exp;;

type aba = {
	delta : succ_states AbaStMap.t;
	start : aba_state;
	final : AbaStSet.t;
}


(** Outputs a PslABA state as a string *)
let rec state2str (q:aba_state) : string = match q with
	| Prop s -> s
	| NProp s -> "! "^s
	| OpAnd (p1, p2) -> "("^(state2str p1)^" & "^(state2str p2)^")"
	| OpOr (p1, p2) -> "("^(state2str p1)^" | "^(state2str p2)^")"
	| OpX p -> "X"^(state2str p)
	| OpY p -> "Y"^(state2str p)
	| OpZ p -> "Z"^(state2str p)
	| OpF p -> "F"^(state2str p)
	| OpG p -> "G"^(state2str p)
	| OpO p -> "O"^(state2str p)
	| OpH p -> "H"^(state2str p)
	| OpU (p1, p2) -> "("^(state2str p1)^" U "^(state2str p2)^")"
	| OpS (p1, p2) -> "("^(state2str p1)^" S "^(state2str p2)^")"
	| OpR (p1, p2) -> "("^(state2str p1)^" R "^(state2str p2)^")"
	| OpT (p1, p2) -> "("^(state2str p1)^" T "^(state2str p2)^")"
	| Fby (i, re, phi) -> "("^(string_of_int i)^", "^(Psl.sere2str re)^" <>-> "^(state2str phi)^")"
	| BFby (i, re, phi) -> "("^(string_of_int i)^", "^(Psl.sere2str re)^" <->-> "^(state2str phi)^")"
	| Trig (i, re, phi) -> "("^(string_of_int i)^", "^(Psl.sere2str re)^" []-> "^(state2str phi)^")"
	| BTrig (i, re, phi) -> "("^(string_of_int i)^", "^(Psl.sere2str re)^" [-]-> "^(state2str phi)^")"
	| Cl (i, re) -> "Cl("^(string_of_int i)^", "^(Psl.sere2str re)^")"
	| NCl (i, re) -> "NCl("^(string_of_int i)^", "^(Psl.sere2str re)^")"



(** Outputs a PslABA as a string *)
let toStr (aut:aba) : string =
	let rec boolstate2str (bqs:succ_states) : string = match bqs with
		| Psl.True -> "tt"
		| Psl.False -> "ff"
		| Psl.Atom (Pr s) -> s
		| Psl.Atom (NPr s) -> "!"^s
		| Psl.Atom (St (p, Back)) -> "<"^(state2str p)^", -1>"
		| Psl.Atom (St (p, WBack)) -> "<"^(state2str p)^", ~1>"
		| Psl.Atom (St (p, Eps)) -> "<"^(state2str p)^", 0>"
		| Psl.Atom (St (p, Forw)) -> "<"^(state2str p)^", 1>"
		| Psl.Neg p -> "! "^(boolstate2str p)
		| Psl.And (p1, p2) -> "("^(boolstate2str p1)^" & "^(boolstate2str p2)^")"
		| Psl.Or (p1, p2) -> "("^(boolstate2str p1)^" | "^(boolstate2str p2)^")"
	in

	let print_trans (q:aba_state) (bqs:succ_states) : string =
		"  "^(state2str q)^" -> "^(boolstate2str bqs)^"\n";
	in
	
	let s = ref "" in
	s := !s^"PslABA:\n";
	s := !s^"Transitions:\n";
	AbaStMap.iter
		(fun q qs -> s := !s^(print_trans q qs))
		aut.delta; 
	s := !s^"Initial state:\n";
	s := !s^"  "^(state2str aut.start)^"\n";
	s := !s^"Final states:\n";
	AbaStMap.iter
		(fun q _ -> 
			if AbaStSet.mem q aut.final then s := !s^"  "^(state2str q)^"\n")
		aut.delta;
	!s^"\n\n"


(** Constructs a 2-way ABA from a RTL formula. 
	@param tt is a string that describes the true value in nuSMV, SPIN, ... 
	@param tt is a string that describes the true value in nuSMV, SPIN, ... 
	@return a pslaba	*)
(* Steps: *)
(* 1. mapping from SERE subformulas to NFAs/NBAs *)
(* 2. compute all reachable states and store the states and its *)
(*    outgoing transitions.*)
let fromPsl (formula:Psl.psl) (tt:string) (ff:string) : aba =
	(* Convert into negated normal form *)
	(* To ensure that there is at least one infinite path! *)
(*	let phi = Psl.toNnf (Psl.OpAnd (Psl.OpG (Psl.Prop "TRUE"), formula)) in  *)
	let phi = Psl.toNnf (formula) in
	
	let dummy = Psl.Prop "00" in  (* for the mapping: SERE -> NFAs *)

	(* Maps SEREs from <>->, []->, Cl, NCl subformulas to NFAs *)
	let re2nfa : nfa Psl.PhiMap.t =
		let add_mapping (psi:psl) (map:nfa Psl.PhiMap.t) : nfa Psl.PhiMap.t = match psi with
			| Psl.Fby (re,_)  	      (* NFAs for Fby and Trig share memory *)
			| Psl.Trig (re,_) -> 
					Psl.PhiMap.add (Psl.Fby (re, dummy)) (Nfa.fromSere re) map
			| Psl.BFby (re, _)        (* NFAs for BFby and BTrig share memory *)
			| Psl.BTrig (re, _) -> 
					Psl.PhiMap.add (Psl.BFby (re, dummy)) (Nfa.mirror (Nfa.fromSere re)) map
			| Psl.Cl re -> Psl.PhiMap.add (Psl.Cl re) (Nfa.toNba re) map  (* !! not implemented jet !! *)
			| Psl.NCl re -> Psl.PhiMap.add (Psl.NCl re) (Nfa.toNegNba re) map  (* !! not implemented jet !! *)
			| _ -> map
		in
		Psl.PhiSet.fold add_mapping (Psl.getSub phi) Psl.PhiMap.empty
	in
	
	(* type conversion of alphabet: from nfa successors to aba successors *)
	let rec conv_gate (g:Psl.bexp) : succ_states = match g with
		| Psl.True -> Psl.True 
		| Psl.False -> Psl.False
		| Psl.Atom s -> Psl.Atom (Pr s) 
		| Psl.Neg h -> Psl.Neg (conv_gate h)
		| Psl.And (h, k) -> Psl.And (conv_gate h, conv_gate k)
		| Psl.Or (h, k) -> Psl.Or (conv_gate h, conv_gate k)
	in
	
	(* Computes the bool. comb. of successors *)
	let rec delta_old (p:aba_state) : succ_states = 
		match p with
			| Prop s -> Psl.Atom (Pr s) 
			| NProp s -> Psl.Atom (NPr s)
			(*| OpAnd (q, r) -> Psl.And (delta_old q, delta_old r)*)
			(*| OpAnd (q, r) -> 
				let q' = match delta_old q with
					| Psl.Atom (St (q'', Back)) -> Psl.Atom (St (OpY q'', Eps))
					| Psl.Atom (St (q'', WBack)) -> Psl.Atom (St (OpZ q'', Eps))
					| _ -> delta_old q
				in
				let r' = match delta_old r with
					| Psl.Atom (St (r'', Back)) -> Psl.Atom (St (OpY r'', Eps))
					| Psl.Atom (St (r'', WBack)) -> Psl.Atom (St (OpZ r'', Eps))
					| _ -> delta_old r
				in
				Psl.And (q', r')*)
			| OpAnd (q, r) -> 
				let q' = match q with	
					| Prop _ 
					| NProp _ -> delta_old q 
					| _ -> Psl.Atom (St (q, Eps)) 
				in
				let r' = match r with	
					| Prop _ 
					| NProp _ -> delta_old r 
					| _ -> Psl.Atom (St (r, Eps)) 
				in
				Psl.And (q', r')
			(*| OpOr (q, r) -> Psl.Or (delta_old q, delta_old r)*)
			(*| OpOr (q, r) -> 
				let q' = match delta_old q with
					| Psl.Atom (St (q'', Back)) -> Psl.Atom (St (OpY q'', Eps))
					| Psl.Atom (St (q'', WBack)) -> Psl.Atom (St (OpZ q'', Eps))
					| _ -> delta_old q
				in
				let r' = match delta_old r with
					| Psl.Atom (St (r'', Back)) -> Psl.Atom (St (OpY r'', Eps))
					| Psl.Atom (St (r'', WBack)) -> Psl.Atom (St (OpZ r'', Eps))
					| _ -> delta_old r
				in
				Psl.Or (q', r')
			*)
			| OpOr (q, r) -> 
				let q' = match q with	
					| Prop _ 
					| NProp _ -> delta_old q 
					| _ -> Psl.Atom (St (q, Eps)) 
				in
				let r' = match r with	
					| Prop _ 
					| NProp _ -> delta_old r 
					| _ -> Psl.Atom (St (r, Eps)) 
				in
				Psl.Or (q', r')
			| OpX q -> Psl.Atom (St (q, Forw))
			| OpY q -> Psl.Atom (St (q, Back))
			| OpZ q -> Psl.Atom (St (q, WBack))
			| OpF q -> delta_old (OpOr (q, OpX(p)))
			| OpG q -> delta_old (OpAnd (q, OpX(p)))
			| OpO q -> delta_old (OpOr (q, OpY(p)))
			| OpH q -> delta_old (OpAnd (q, OpZ(p))) 
			| OpU (q1, q2) -> delta_old (OpOr (q2, OpAnd (q1, OpX(p))))
			| OpR (q1, q2) -> delta_old (OpAnd (q2, OpOr (q1, OpX(p))))
			| OpS (q1, q2) -> delta_old (OpOr (q2, OpAnd (q1, OpY(p))))
			| OpT (q1, q2) -> delta_old (OpAnd (q2, OpOr (q1, OpZ(p)))) 
			| Cl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.Cl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.Or (qs, Psl.And (conv_gate gate, Psl.Atom (St (Cl (q, re), Forw))))
				in
				List.fold_left trans Psl.False succs 
			| NCl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.NCl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.And (qs, Psl.And (conv_gate gate, Psl.Atom (St (NCl (q, re), Forw))))
				in
				List.fold_left trans Psl.True succs 
			| Fby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.False	in 
					let fby = Psl.Atom (St (Fby (q, re, phi), Forw)) in
					Psl.Or(qs, Psl.And (g', Psl.Or(fby, out)))
				in
				List.fold_left trans Psl.False succs
			| BFby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.False	in 
					let bfby = Psl.Atom (St (BFby (q, re, phi), Back)) in
					Psl.Or (qs, Psl.And (g', Psl.Or(bfby, out)))
				in
				List.fold_left trans Psl.False succs
			| Trig (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.True	in 
					let trig = Psl.Atom (St (Trig (q, re, phi), Forw)) in
					Psl.And(qs, Psl.Or (Psl.Neg g', Psl.And(trig, out)))
				in
				List.fold_left trans Psl.True succs
			| BTrig (i, re, phi) ->  
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.True	in 
					let btrig = Psl.Atom (St (BTrig (q, re, phi), WBack)) in
					Psl.And(qs, Psl.Or (Psl.Neg g', Psl.And(btrig, out)))
				in
				List.fold_left trans Psl.True succs 
	in



	(* Converts a gate into an aba_sate in NNF *)
	let rec gate2nnf (g:Psl.bexp) : aba_state = match g with
		| Psl.True -> Prop tt
		| Psl.False -> Prop ff
		| Psl.Atom s -> Prop s
		| Psl.Neg ex -> 	
			begin match ex with
				| Psl.True -> Prop ff
				| Psl.False -> Prop tt
				| Psl.Atom s -> NProp s
				| Psl.Neg ex1 -> gate2nnf ex1
				| Psl.And (ex1, ex2) -> OpOr (gate2nnf (Psl.Neg ex1), gate2nnf (Psl.Neg ex2))
				| Psl.Or (ex1, ex2) -> OpAnd (gate2nnf (Psl.Neg ex1), gate2nnf (Psl.Neg ex2))
			end
		| Psl.And (ex1, ex2) -> OpAnd (gate2nnf ex1, gate2nnf ex2)
		| Psl.Or (ex1, ex2) -> OpOr (gate2nnf ex1, gate2nnf ex2)
	in

	(* Computes the bool. comb. of successors *)
	let delta (state:aba_state) : succ_states = 
		(* Info whether there exists a backward/forward/both tuple *)
		let mult (a:int) (b:int) = match (a,b) with
			| (0,0) -> 0
			| (0,-1) | (-1,0) | (-1,-1) -> -1
			| (0,1) | (1,0) | (1,1) -> 1
			| _ -> 2
		in
		let rec delta' (p:aba_state) : (succ_states * int) = match p with
			| Prop s -> (Psl.Atom (Pr s), 0) 
			| NProp s -> (Psl.Atom (NPr s), 0)
			| OpAnd (q, r) -> 
				let (q', a) = delta' q in 
				let (r', b) = delta' r in 
				(Psl.And (q', r'), mult a b)
			| OpOr (q, r) -> 
				let (q', a) = delta' q in 
				let (r', b) = delta' r in 
				let m = mult a b in
				if m=2 then	
					(Psl.Or (Psl.Atom (St (q, Eps)), r'), b) 
				else
					(Psl.Or (q', r'), m) 
			| OpX q -> (Psl.Atom (St (q, Forw)), 1)
			| OpY q -> (Psl.Atom (St (q, Back)), -1)
			| OpZ q -> (Psl.Atom (St (q, WBack)), -1)
			| OpF q -> delta' (OpOr (q, OpX(p)))
			| OpG q -> delta' (OpAnd (q, OpX(p)))
			| OpO q -> delta' (OpOr (q, OpY(p)))
			| OpH q -> delta' (OpAnd (q, OpZ(p))) 
			| OpU (q1, q2) -> delta' (OpOr (q2, OpAnd (q1, OpX(p))))
			| OpR (q1, q2) -> delta' (OpAnd (q2, OpOr (q1, OpX(p))))
			| OpS (q1, q2) -> delta' (OpOr (q2, OpAnd (q1, OpY(p))))
			| OpT (q1, q2) -> delta' (OpAnd (q2, OpOr (q1, OpZ(p)))) 
			| Cl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.Cl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.Or (qs, Psl.And (conv_gate gate, Psl.Atom (St (Cl (q, re), Forw))))
				in
				(List.fold_left trans Psl.False succs, 1) 
			| NCl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.NCl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.And (qs, Psl.And (conv_gate gate, Psl.Atom (St (NCl (q, re), Forw))))
				in
				(List.fold_left trans Psl.True succs, 1) 
			| Fby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (bqs:aba_state) (gate, q) : aba_state =
					let g' = gate2nnf gate in
					let out = if nfa.Nfa.final.(q) then phi else Prop ff in 
					let fby = OpX (Fby (q, re, phi)) in
					OpOr(bqs, OpAnd (g', OpOr(out, fby)))
				in
				delta' (List.fold_left trans (Prop ff) succs)
			| BFby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (bqs:aba_state) (gate, q) : aba_state =
					let g' = gate2nnf gate in
					let out = if nfa.Nfa.final.(q) then phi else Prop ff in 
					let bfby = OpY (BFby (q, re, phi)) in
					OpOr (bqs, OpAnd (g', OpOr(out, bfby)))
				in
				delta' (List.fold_left trans (Prop ff) succs)
			| Trig (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (bqs:aba_state) (gate, q) : aba_state =
					let g'_neg = gate2nnf (Psl.Neg gate) in
					let out = if nfa.Nfa.final.(q) then phi else Prop tt in 
					let trig = OpX (Trig (q, re, phi)) in
					OpAnd(bqs, OpOr (g'_neg, OpAnd(out, trig)))
				in
				delta' (List.fold_left trans (Prop tt) succs)
			| BTrig (i, re, phi) ->  
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (bqs:aba_state) (gate, q) : aba_state =
					let g'_neg = gate2nnf (Psl.Neg gate) in
					let out = if nfa.Nfa.final.(q) then phi else Prop tt in 
					let btrig = OpZ (BTrig (q, re, phi)) in
					OpAnd(bqs, OpOr (g'_neg, OpAnd(out, btrig)))
				in
				delta' (List.fold_left trans (Prop tt) succs)
			in
			fst (delta' state)  
	in
	
			
	(* extracts the states and leaves the symbols from the bool. comb. of the successors *)
	let rec get_states (bqs:succ_states) : aba_state list = match bqs with
		| Psl.True 
		| Psl.False
		| Psl.Atom (Pr _)
		| Psl.Atom (NPr _) -> []
		| Psl.Atom (St (q, _)) -> [q]  
		| Psl.Neg bqs1 -> get_states bqs1 
		| Psl.And (bqs1, bqs2) -> List.rev_append (get_states bqs1) (get_states bqs2)
		| Psl.Or (bqs1, bqs2) -> List.rev_append (get_states bqs1) (get_states bqs2)
	in
	
	(* ==================================================== *)
	(* HERE WE CAN OPTIMIZE TO OBTAIN LESS ACCEPTING STATES *)
	(* e.g. only until formulas + some SERE automata states *)
	(* are not final *)
	(* ==================================================== *)
	(* checks whether a state is final or not *)
	let is_final (p:aba_state) : bool = match p with
		| OpG _
		| OpR _ 
		| Trig _ 
		| BTrig _ -> true   
		| Cl (i, re) ->
			let nba : Nfa.nfa = Psl.PhiMap.find (Psl.Cl re) re2nfa in
			nba.Nfa.final.(i)
		| NCl (i, re) ->
			let nba : Nfa.nfa = Psl.PhiMap.find (Psl.NCl re) re2nfa in
			nba.Nfa.final.(i)
		| _ -> false
	in

	(* convert the _formula_ phi to the inital _state_ of the ABA *)
	let rec phi2init (phi:psl) : aba_state = match phi with
		| Psl.Prop s -> Prop s 
		| Psl.NProp s -> NProp s
		| Psl.OpNeg psi -> failwith "aba: Formula not in NNF"
		| Psl.OpAnd (psi1, psi2) -> OpAnd (phi2init psi1, phi2init psi2)
		| Psl.OpOr (psi1, psi2) -> OpOr (phi2init psi1, phi2init psi2)
		| Psl.OpX psi -> OpX (phi2init psi)
		| Psl.OpY psi -> OpY (phi2init psi)
		| Psl.OpZ psi -> OpZ (phi2init psi)
		| Psl.OpF psi -> OpF (phi2init psi)
		| Psl.OpG psi -> OpG (phi2init psi)
		| Psl.OpO psi -> OpO (phi2init psi)
		| Psl.OpH psi -> OpH (phi2init psi)
		| Psl.OpU (psi1, psi2) -> OpU (phi2init psi1, phi2init psi2)
		| Psl.OpS (psi1, psi2) -> OpS (phi2init psi1, phi2init psi2)
		| Psl.OpR (psi1, psi2) -> OpR (phi2init psi1, phi2init psi2)
		| Psl.OpT (psi1, psi2) -> OpT (phi2init psi1, phi2init psi2)
		| Psl.Fby (re, psi) -> 
				let nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				Fby (nfa.Nfa.start, re, phi2init psi)
		| Psl.BFby (re, psi) -> 
				let nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				BFby (nfa.Nfa.start, re, phi2init psi)
		| Psl.Trig (re, psi) ->
				let nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				Trig (nfa.Nfa.start, re, phi2init psi)
		| Psl.BTrig (re, psi) ->
				let nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				BTrig (nfa.Nfa.start, re, phi2init psi)
		| Psl.Cl re ->
				let nba = Psl.PhiMap.find (Psl.Cl re) re2nfa in
				Cl (nba.Nfa.start, re)
		| Psl.NCl re ->
				let nba = Psl.PhiMap.find (Psl.NCl re) re2nfa in
				NCl (nba.Nfa.start, re)
	in
	
	(* Initial state *)
	let ini : aba_state = phi2init phi in

	(* Stores all reached states with their outgoing transitions *)
	let del = ref AbaStMap.empty in   
	let rec traverse (p:aba_state) = 
		let succs = delta p in
		del := AbaStMap.add p succs !del;
		
		let qs = get_states succs in 
		List.iter (fun q -> if not (AbaStMap.mem q !del) then traverse q) qs; 
	in
	traverse ini;

	(* Compute final states *)
	let fin = ref AbaStSet.empty in
	AbaStMap.iter
		(fun q _ -> if is_final q then fin := AbaStSet.add q !fin)
		!del;

	{delta=(!del); start=ini; final=(!fin)};;












(* Old and not optimized version of fromPsl *)
(* Steps: *)
(* 1. mapping from SERE subformulas to NFAs/NBAs *)
(* 2. compute all reachable states and store the states and its *)
(*    outgoing transitions.*)
let fromPslOld (formula:Psl.psl) : aba =
	let phi = Psl.toNnf formula in  (* Convert into negated normal form *)
	let dummy = Psl.Prop "00" in  (* for the mapping: SERE -> NFAs *)

	(* Maps SEREs from <>->, []->, Cl, NCl subformulas to NFAs *)
	let re2nfa : nfa Psl.PhiMap.t =
		let add_mapping (psi:psl) (map:nfa Psl.PhiMap.t) : nfa Psl.PhiMap.t = match psi with
			| Psl.Fby (re,_)  	      (* NFAs for Fby and Trig share memory *)
			| Psl.Trig (re,_) -> 
					Psl.PhiMap.add (Psl.Fby (re, dummy)) (Nfa.fromSere re) map
			| Psl.BFby (re, _)        (* NFAs for BFby and BTrig share memory *)
			| Psl.BTrig (re, _) -> 
					Psl.PhiMap.add (Psl.BFby (re, dummy)) (Nfa.mirror (Nfa.fromSere re)) map
			| Psl.Cl re -> 
					failwith "The CL operator will be implemented soon."
					(*Psl.PhiMap.add (Psl.Cl re) (Nfa.toNba re) map*)  (* !! not implemented jet !! *)
			| Psl.NCl re -> 
					(* Psl.PhiMap.add (Psl.NCl re) (Nfa.toNegNba re) map*)  (* !! not implemented jet !! *)
					failwith "The CL operator will be implemented soon."
			| _ -> map
		in
		Psl.PhiSet.fold add_mapping (Psl.getSub phi) Psl.PhiMap.empty
	in
	
	(* type conversion of alphabet: from nfa successors to aba successors *)
	let rec conv_gate (g:Psl.bexp) : succ_states = match g with
		| Psl.True -> Psl.True 
		| Psl.False -> Psl.False
		| Psl.Atom s -> Psl.Atom (Pr s) 
		| Psl.Neg h -> Psl.Neg (conv_gate h)
		| Psl.And (h, k) -> Psl.And (conv_gate h, conv_gate k)
		| Psl.Or (h, k) -> Psl.Or (conv_gate h, conv_gate k)
	in
	
	(* Computes the bool. comb. of successors *)
	let rec delta (p:aba_state) : succ_states = 
		match p with
			| Prop s -> Psl.Atom (Pr s) 
			| NProp s -> Psl.Atom (NPr s)
			(*| OpAnd (q, r) -> Psl.And (delta q, delta r)*)
			(*| OpAnd (q, r) -> 
				let q' = match delta q with
					| Psl.Atom (St (q'', Back)) -> Psl.Atom (St (OpY q'', Eps))
					| Psl.Atom (St (q'', WBack)) -> Psl.Atom (St (OpZ q'', Eps))
					| _ -> delta q
				in
				let r' = match delta r with
					| Psl.Atom (St (r'', Back)) -> Psl.Atom (St (OpY r'', Eps))
					| Psl.Atom (St (r'', WBack)) -> Psl.Atom (St (OpZ r'', Eps))
					| _ -> delta r
				in
				Psl.And (q', r')*)
			| OpAnd (q, r) -> 
				let q' = match q with	
					| Prop _ 
					| NProp _ -> delta q 
					| _ -> Psl.Atom (St (q, Eps)) 
				in
				let r' = match r with	
					| Prop _ 
					| NProp _ -> delta r 
					| _ -> Psl.Atom (St (r, Eps)) 
				in
				Psl.And (q', r')
			(*| OpOr (q, r) -> Psl.Or (delta q, delta r)*)
			(*| OpOr (q, r) -> 
				let q' = match delta q with
					| Psl.Atom (St (q'', Back)) -> Psl.Atom (St (OpY q'', Eps))
					| Psl.Atom (St (q'', WBack)) -> Psl.Atom (St (OpZ q'', Eps))
					| _ -> delta q
				in
				let r' = match delta r with
					| Psl.Atom (St (r'', Back)) -> Psl.Atom (St (OpY r'', Eps))
					| Psl.Atom (St (r'', WBack)) -> Psl.Atom (St (OpZ r'', Eps))
					| _ -> delta r
				in
				Psl.Or (q', r')
			*)
			| OpOr (q, r) -> 
				let q' = match q with	
					| Prop _ 
					| NProp _ -> delta q 
					| _ -> Psl.Atom (St (q, Eps)) 
				in
				let r' = match r with	
					| Prop _ 
					| NProp _ -> delta r 
					| _ -> Psl.Atom (St (r, Eps)) 
				in
				Psl.Or (q', r')
			| OpX q -> Psl.Atom (St (q, Forw))
			| OpY q -> Psl.Atom (St (q, Back))
			| OpZ q -> Psl.Atom (St (q, WBack))
			| OpF q -> delta (OpOr (q, OpX(p)))
			| OpG q -> delta (OpAnd (q, OpX(p)))
			| OpO q -> delta (OpOr (q, OpY(p)))
			| OpH q -> delta (OpAnd (q, OpZ(p))) 
			| OpU (q1, q2) -> delta (OpOr (q2, OpAnd (q1, OpX(p))))
			| OpR (q1, q2) -> delta (OpAnd (q2, OpOr (q1, OpX(p))))
			| OpS (q1, q2) -> delta (OpOr (q2, OpAnd (q1, OpY(p))))
			| OpT (q1, q2) -> delta (OpAnd (q2, OpOr (q1, OpZ(p)))) 
			| Cl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.Cl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.Or (qs, Psl.And (conv_gate gate, Psl.Atom (St (Cl (q, re), Forw))))
				in
				List.fold_left trans Psl.False succs 
			| NCl (i, re) ->
				let nba:Nfa.nfa = Psl.PhiMap.find (Psl.NCl re) re2nfa in
				let succs = nba.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					Psl.And (qs, Psl.And (conv_gate gate, Psl.Atom (St (NCl (q, re), Forw))))
				in
				List.fold_left trans Psl.True succs 
			| Fby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.False	in 
					let fby = Psl.Atom (St (Fby (q, re, phi), Forw)) in
					Psl.Or(qs, Psl.And (g', Psl.Or(fby, out)))
				in
				List.fold_left trans Psl.False succs
			| BFby (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.False	in 
					let bfby = Psl.Atom (St (BFby (q, re, phi), Back)) in
					Psl.Or (qs, Psl.And (g', Psl.Or(bfby, out)))
				in
				List.fold_left trans Psl.False succs
			| Trig (i, re, phi) ->
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.True	in 
					let trig = Psl.Atom (St (Trig (q, re, phi), Forw)) in
					Psl.And(qs, Psl.Or (Psl.Neg g', Psl.And(trig, out)))
				in
				List.fold_left trans Psl.True succs
			| BTrig (i, re, phi) ->  
				let nfa:Nfa.nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				let succs = nfa.Nfa.delta.(i) in
				let trans (qs:succ_states) (gate, q) : succ_states =
					let g' = conv_gate gate in
					let out = if nfa.Nfa.final.(q) then Psl.Atom (St (phi, Eps)) else Psl.True	in 
					let btrig = Psl.Atom (St (BTrig (q, re, phi), WBack)) in
					Psl.And(qs, Psl.Or (Psl.Neg g', Psl.And(btrig, out)))
				in
				List.fold_left trans Psl.True succs 
	in

	(* extracts the states and leaves the symbols from the bool. comb. of the successors *)
	let rec get_states (bqs:succ_states) : aba_state list = match bqs with
		| Psl.True 
		| Psl.False
		| Psl.Atom (Pr _)
		| Psl.Atom (NPr _) -> []
		| Psl.Atom (St (q, _)) -> [q]  
		| Psl.Neg bqs1 -> get_states bqs1 
		| Psl.And (bqs1, bqs2) -> List.rev_append (get_states bqs1) (get_states bqs2)
		| Psl.Or (bqs1, bqs2) -> List.rev_append (get_states bqs1) (get_states bqs2)
	in
	
	(* ==================================================== *)
	(* HERE WE CAN OPTIMIZE TO OBTAIN LESS ACCEPTING STATES *)
	(* e.g. only until formulas + some SERE automata states *)
	(* are not final *)
	(* ==================================================== *)
	(* checks whether a state is final or not *)
	let is_final (p:aba_state) : bool = match p with
		| OpG _
		| OpR _ 
		| Trig _ 
		| BTrig _ -> true   
		| Cl (i, re) ->
			let nba : Nfa.nfa = Psl.PhiMap.find (Psl.Cl re) re2nfa in
			nba.Nfa.final.(i)
		| NCl (i, re) ->
			let nba : Nfa.nfa = Psl.PhiMap.find (Psl.NCl re) re2nfa in
			nba.Nfa.final.(i)
		| _ -> false
	in

	(* convert the _formula_ phi to the inital _state_ of the ABA *)
	let rec phi2init (phi:psl) : aba_state = match phi with
		| Psl.Prop s -> Prop s 
		| Psl.NProp s -> NProp s
		| Psl.OpNeg psi -> failwith "aba: Formula not in NNF"
		| Psl.OpAnd (psi1, psi2) -> OpAnd (phi2init psi1, phi2init psi2)
		| Psl.OpOr (psi1, psi2) -> OpOr (phi2init psi1, phi2init psi2)
		| Psl.OpX psi -> OpX (phi2init psi)
		| Psl.OpY psi -> OpY (phi2init psi)
		| Psl.OpZ psi -> OpZ (phi2init psi)
		| Psl.OpF psi -> OpF (phi2init psi)
		| Psl.OpG psi -> OpG (phi2init psi)
		| Psl.OpO psi -> OpO (phi2init psi)
		| Psl.OpH psi -> OpH (phi2init psi)
		| Psl.OpU (psi1, psi2) -> OpU (phi2init psi1, phi2init psi2)
		| Psl.OpS (psi1, psi2) -> OpS (phi2init psi1, phi2init psi2)
		| Psl.OpR (psi1, psi2) -> OpR (phi2init psi1, phi2init psi2)
		| Psl.OpT (psi1, psi2) -> OpT (phi2init psi1, phi2init psi2)
		| Psl.Fby (re, psi) -> 
				let nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				Fby (nfa.Nfa.start, re, phi2init psi)
		| Psl.BFby (re, psi) -> 
				let nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				BFby (nfa.Nfa.start, re, phi2init psi)
		| Psl.Trig (re, psi) ->
				let nfa = Psl.PhiMap.find (Psl.Fby (re, dummy)) re2nfa in
				Trig (nfa.Nfa.start, re, phi2init psi)
		| Psl.BTrig (re, psi) ->
				let nfa = Psl.PhiMap.find (Psl.BFby (re, dummy)) re2nfa in
				BTrig (nfa.Nfa.start, re, phi2init psi)
		| Psl.Cl re ->
				let nba = Psl.PhiMap.find (Psl.Cl re) re2nfa in
				Cl (nba.Nfa.start, re)
		| Psl.NCl re ->
				let nba = Psl.PhiMap.find (Psl.NCl re) re2nfa in
				NCl (nba.Nfa.start, re)
	in
	
	(* Initial state *)
	let ini : aba_state = phi2init phi in

	(* Stores all reached states with their outgoing transitions *)
	let del = ref AbaStMap.empty in   
	let rec traverse (p:aba_state) = 
		let succs = delta p in
		del := AbaStMap.add p succs !del;
		
		let qs = get_states succs in 
		List.iter (fun q -> if not (AbaStMap.mem q !del) then traverse q) qs; 
	in
	traverse ini;

	(* Compute final states *)
	let fin = ref AbaStSet.empty in
	AbaStMap.iter
		(fun q _ -> if is_final q then fin := AbaStSet.add q !fin)
		!del;

	{delta=(!del); start=ini; final=(!fin)};;





(** Converts a PslABA into an ABA with integers as states *)
let toAba (aut:aba) : Aba.aba =
	(* maps states to integers *)
	let n = ref 0 in
	let st2int = ref AbaStMap.empty in
	AbaStMap.iter
		(fun q _ -> 
				st2int := AbaStMap.add q !n !st2int;
				n := !n + 1)		
		aut.delta;
	
	(* rename successors + construct ABA: see also let get_states = ... *)
	let rec ren_succs (bqs:succ_states) : Aba.succ_states = match bqs with
		| Psl.True -> Psl.True 
		| Psl.False -> Psl.False
		| Psl.Atom (Pr s) -> Psl.Atom (Aba.Pr s)
		| Psl.Atom (NPr s) -> Psl.Atom (Aba.NPr s)
		| Psl.Atom (St (p, d)) -> 
				let d' = match d with 
					| WBack -> Aba.WBack
					| Back -> Aba.Back
					| Eps -> Aba.Eps
					| Forw -> Aba.Forw
				in
				Psl.Atom (Aba.St (AbaStMap.find p !st2int, d'))
		| Psl.Neg p -> Psl.Neg (ren_succs p)
		| Psl.And (p, p') -> Psl.And (ren_succs p, ren_succs p')
		| Psl.Or (p, p') -> Psl.Or (ren_succs p, ren_succs p')
	in
	
	let ini = AbaStMap.find aut.start !st2int in  
	let del = Array.make !n Psl.True in  (* transition function of ABA *) 
	let fin = Array.make !n false in  (* final states of ABA *)
	AbaStMap.iter
		(fun q qs ->
			let i = AbaStMap.find q !st2int in
			del.(i) <- ren_succs qs;
			if AbaStSet.mem q aut.final then fin.(i) <- true)
		aut.delta;
		
	{Aba.delta=del; Aba.start=ini; Aba.final=fin};;
	


