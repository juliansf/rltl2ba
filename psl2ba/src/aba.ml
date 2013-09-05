(** Routines to transform alternating Büchi automata (ABAs) *)

(* An ABA consists of states Q, an alphabet S=2^P \cup {true, false},      *)
(* where P is the set of propositions, a transition function: Q -->        *)
(* \calB(P \cup \negP \cup Q)}, and an acceptance set F \subseteq Q.       *)

open Sets

type bexp = Psl.bexp
type sere = Psl.sere
type psl = Psl.psl
type nfa = Nfa.nfa

(** Defines the type of an ABA. The transition function is defined as
delta: states --> \calB(bexp \cup (states \times direction)) *)
type dir = Back | WBack | Eps | Forw  (* directions: back, weak back, epsilon, or forward transition *)
type succ = NPr of string | Pr of string | St of int * dir
type succ_states = succ Psl.bool_exp;;

type aba = {
	delta : succ_states array;
	start : int;
	final : bool array;
}

(* Before we define an 1-way ABA with transition function from int to      *)
(* succ_states, we define an ABA with transition function from             *)
(* "subformulas" to succ_states.                                           *)
type aba_state =
	| Prop of string
	| NProp of string  (* negated prop used in negated normal form *)
	| OpAnd of aba_state * aba_state
	| OpOr of aba_state * aba_state
	| OpX of aba_state
	| OpY of aba_state
	| OpZ of aba_state
	| OpU of aba_state * aba_state
	| OpS of aba_state * aba_state
	| OpR of aba_state * aba_state
	| OpT of aba_state * aba_state
	| Fby of int * sere * aba_state  (* state of NFA given by sere and aba_state *)
	| BFby of int * sere * aba_state
	| Trig of int * sere * aba_state
	| BTrig of int * sere * aba_state
	| Cl of int * sere
	| NCl of int * sere;;

module AbaSt = struct
	type t = aba_state
	let compare = compare
end
module AbaStSet = Set.Make(AbaSt);;
module AbaStMap = Map.Make(AbaSt);;


(** Converts an ABA into a string *)
let toStr (aba: aba) (tt: string) (ff: string) : string =
	(* Converts a bool. comb. of integer states into a string *)
	let rec bqs2str (bqs: succ_states) : string = match bqs with
		| Psl.True -> tt
		| Psl.False -> ff
		| Psl.Atom (Pr s) -> s
		| Psl.Atom (NPr s) -> "! "^s
		| Psl.Atom (St (i, d)) ->
				let d' = match d with
					| Back -> "-1"
					| WBack -> "~1"
					| Eps -> "0"
					| Forw -> "1"
				in
				"<"^(string_of_int i)^","^d'^">"
		| Psl.Neg c -> "! "^(bqs2str c)
		| Psl.And (c, d) -> "("^(bqs2str c)^" & "^(bqs2str d)^")"
		| Psl.Or (c, d) -> "("^(bqs2str c)^" | "^(bqs2str d)^")"
	in
	let sDel = ref "" in
	Array.iteri
		(fun q qs ->
			let fin = if aba.final.(q) then "f " else "  " in
			let inifin = if q=aba.start then "  s"^fin else "   "^fin in
			sDel := !sDel ^inifin^(string_of_int q)^" -> "^(bqs2str qs)^"\n")
		aba.delta;
	(* return *)
	"2-way ABA:\n"^(!sDel)^"\n"



(** Removes unreachable states. *)
let rem_unreachables (aut:aba) : aba =
	let n = Array.length aut.delta in
	let reachable = Array.make n false in
	(* Mark reachable states *)
	reachable.(aut.start) <- true;
	let rec mark (bqs:succ_states) : unit = match bqs with
		| Psl.True
		| Psl.False -> ()
		| Psl.Atom (St (q, _)) -> reachable.(q) <- true
		| Psl.Atom _ -> ()
		| Psl.Neg bqs1 -> mark bqs1
		| Psl.And (bqs1, bqs2) -> (mark bqs1; mark bqs2)
		| Psl.Or (bqs1, bqs2) -> (mark bqs1; mark bqs2)
	in
	Array.iter mark aut.delta;
	(* Calculate how much each state gets decreased = number of empty slots before q *)
	let dec = Array.make n 0 in
	for i=1 to n-1 do
		let inc = if not(reachable.(i-1)) then 1 else 0 in
		dec.(i) <- dec.(i-1) + inc
	done;
	(* Construct new ABA *)
	let n' = Array.fold_left (fun i b -> if b then i+1 else i) 0 reachable in  (* Count reachable states *)
	let del = Array.make n' Psl.True in
	let fin = Array.make n' false in
	let rec rename (bqs:succ_states) : succ_states = match bqs with
		| Psl.True
		| Psl.False -> bqs
		| Psl.Atom (St (q, d)) -> Psl.Atom (St (q-dec.(q), d))
		| Psl.Atom _ -> bqs
		| Psl.Neg bqs1 -> Psl.Neg (rename bqs1)
		| Psl.And (bqs1, bqs2) -> Psl.And (rename bqs1, rename bqs2)
		| Psl.Or (bqs1, bqs2) -> Psl.Or (rename bqs1, rename bqs2)
	in
	for i=0 to n-1 do
		if reachable.(i) then begin
			del.(i-dec.(i)) <- rename aut.delta.(i);
			fin.(i-dec.(i)) <- aut.final.(i)
		end
	done;
	{delta=del; start=aut.start-dec.(aut.start); final=fin}




(** Removes false and true propositions when they are superfluous. *)
let simply_remttff (aut:aba) : aba =
	let rec remttff (bqs:succ_states)	: succ_states = match bqs with
		| Psl.True
		| Psl.False
		| Psl.Atom _ -> bqs
		| Psl.Neg bqs1 ->
				let ret1 = remttff bqs1 in
				begin match ret1 with
					| Psl.True -> Psl.False
				  | Psl.False -> Psl.True
					| _ -> Psl.Neg ret1
				end
		| Psl.And (bqs1, bqs2) ->
				let ret1 = remttff bqs1 in
				let ret2 = remttff bqs2 in
				if ret1=Psl.False || ret2=Psl.False then Psl.False else
				if ret1=Psl.True then ret2 else
				if ret2=Psl.True then ret1 else
				Psl.And (ret1, ret2)
		| Psl.Or (bqs1, bqs2) ->
				let ret1 = remttff bqs1 in
				let ret2 = remttff bqs2 in
				if ret1=Psl.True || ret2=Psl.True then Psl.True else
				if ret1=Psl.False then ret2 else
				if ret2=Psl.False then ret1 else
				Psl.Or (ret1, ret2)
	in
	let del = Array.map remttff aut.delta in
	{delta=del; start=aut.start; final=aut.final}


(** Merges states whose successor consists of a single proposition, true, or false *)
let simply_merge (aut:aba) : aba =
	let s2q = ref StrMap.empty in  (* proposition -> min_state *)
	let q2q = ref IntMap.empty in  (* redirect: state -> min_state *)
	(* store least state of the set of states that direct to same proposition *)
	Array.iteri
		(fun q qs -> match qs with
			| Psl.True ->  (* identify True by Prop "11" *)
					if not(StrMap.mem "11" !s2q)
						then	s2q := StrMap.add "11" q !s2q
						else
							let minq = StrMap.find "11" !s2q in
							q2q := IntMap.add q minq !q2q
			| Psl.False ->  (* identify True by Prop "00" *)
					if not(StrMap.mem "00" !s2q)
						then s2q := StrMap.add "00" q !s2q
						else
							let minq = StrMap.find "00" !s2q in
							q2q := IntMap.add q minq !q2q
			| Psl.Atom (Pr s) ->
					if not(StrMap.mem s !s2q)
						then s2q := StrMap.add s q !s2q
						else
							let minq = StrMap.find s !s2q in
							q2q := IntMap.add q minq !q2q
			| _ -> ())
		aut.delta;
	(* redirect states *)
	let rec redir (bqs:succ_states) : succ_states = match bqs with
		| Psl.True
		| Psl.False -> bqs
		| Psl.Atom (St (q,d)) ->
				if IntMap.mem q !q2q then
					let minq = IntMap.find q !q2q in
					Psl.Atom (St (minq,d))
				else bqs
		| Psl.Atom _ -> bqs
		| Psl.Neg bqs1 -> Psl.Neg (redir bqs1)
		| Psl.And (bqs1, bqs2) -> Psl.And (redir bqs1, redir bqs2)
		| Psl.Or (bqs1, bqs2) -> Psl.Or (redir bqs1, redir bqs2)
	in
	let del = Array.map redir aut.delta in
	rem_unreachables {delta=del; start=aut.start; final=aut.final};;


(** Calls all simplification functions. *)
let simplify (aut:aba) (tt:string) (ff:string) : aba =
	(* Replace (Pr tt) and (Pr ff) by True and False, respectively. *)
	let rec subst (bqs:succ_states) : succ_states = match bqs with
		| Psl.True
		| Psl.False -> bqs
		| Psl.Atom (Pr s) ->
				if s=tt then Psl.True else
					if s=ff then Psl.False else
						bqs
		| Psl.Atom (NPr s) ->
				if s=tt then Psl.False else
					if s=ff then Psl.True else
						bqs
		| Psl.Atom _ -> bqs
		| Psl.Neg bqs1 -> Psl.Neg(subst bqs1)
		| Psl.And (bqs1, bqs2) -> Psl.And (subst bqs1, subst bqs2)
		| Psl.Or (bqs1, bqs2) -> Psl.Or (subst bqs1, subst bqs2)
	in
	let del = Array.map subst aut.delta in
	let aut1 = {delta=del; start=aut.start; final=aut.final} in
	let aut2 = simply_remttff aut1 in
	let aut3 = simply_merge aut2 in
	aut3;;


(** Extracts the underlying graph from an ABA *)
let toGraph (aut:aba) : Graph.graph =
	let rec get_succs (bqs:succ_states)	: int list = match bqs with
		| Psl.True
		| Psl.False
		| Psl.Atom Pr _
		| Psl.Atom NPr _ -> []
		| Psl.Atom (St (q, _)) -> [q]
		| Psl.Neg bqs1 -> get_succs bqs1
		| Psl.And (bqs1, bqs2)
		| Psl.Or (bqs1, bqs2) -> (get_succs bqs1) @ (get_succs bqs2)
	in
	let succ = Array.map get_succs aut.delta in  (* extract underlying graph *)
	let succ' = Array.map Sets.fromList succ in  (* succ set instead of succ list *)
	Array.map (fun l -> Sets.fromList l) succ'   (* convert succ lists to succ sets *)




(** Checks whether the automaton is very weak, i.e. all loops have size one *)
let isVeryWeak (aut:aba) : bool =
	let succ = toGraph aut in
	let sccs = Graph.sccs succ in
	let succL = Array.to_list sccs in
	let succS = Sets.fromList succL in
	(* aut is very weak if each state is mapped to a different SCC *)
	if List.length succL = List.length succS then true else false





(** Converts a 2-way ABA into a nuSMV program (Dax/Klaedtke/Lange)
	@param dgo disables Gastin/Oddoux construction if true
*)
(* type of substitutions for states and for symbols *)
type subst = ByFalse | ByTrue | ByRState | ByRState' | BySState | BySState' | ByFinState | ByFinState'
type subst' = BySym | BySym'

let toSmv (aut:aba) (pref:string) (dgo:bool) : Smv.smv =
	(* Returns the list [0; 1; 2; ...; n-1] *)
	let nList (n:int) : int list = Array.to_list (Array.init n (fun i -> i)) in
	(* Converts an array to an association list *)
	let arr2ali (arr:'a array) = List.combine (nList (Array.length arr)) (Array.to_list arr)	in

	let n = Array.length aut.delta in
	let allL = nList n in
	(* Returns the list of final states *)
	let nonfinalsL : int list =
		let allAli = (arr2ali aut.final) in
		let finAli = List.filter (fun (_, b) -> not(b)) allAli in
		fst (List.split finAli)
	in

	(* Substitutes a state according to its direction *)
	let subst_st ((q, d):int*dir) (s0:subst) (s1:subst) (s2:subst) (s3:subst) : Smv.varexp =
		let subst2 (s:subst) : Smv.varexp = match s with
			| ByFalse -> Smv.False
			| ByTrue -> Smv.True
			| ByRState -> Smv.Var (pref^"r"^(string_of_int q))
			| BySState -> Smv.Var (pref^"s"^(string_of_int q))
			| ByRState' -> Smv.NextVar (pref^"r"^(string_of_int q))
			| BySState' -> Smv.NextVar (pref^"s"^(string_of_int q))
			| ByFinState -> begin match not(aut.final.(q)) with
				| true -> Smv.Var (pref^"s"^(string_of_int q))
				| false -> Smv.Var (pref^"r"^(string_of_int q))
				end
			| ByFinState' -> begin match not(aut.final.(q)) with
				| true -> Smv.NextVar (pref^"s"^(string_of_int q))
				| false -> Smv.NextVar (pref^"r"^(string_of_int q))
				end
		in
		match d with
			| Back -> subst2 s0
			| WBack -> subst2 s1
			| Eps -> subst2 s2
			| Forw -> subst2 s3
	in
	(* Substitutes successor states (bool. comb. of states). *)
	(* s0..s3 are the substitutions for the Back, WBack, Eps, and Forw states.*)
	(* s4 is the substitution for the propositions. *)
	let rec subst_succs (bqs:succ_states) (s0:subst) (s1:subst) (s2:subst) (s3:subst) (s4:subst') : Smv.varexp = match bqs with
		|	Psl.True -> Smv.True
		| Psl.False -> Smv.False
		| Psl.Atom (Pr s) -> begin match s4 with
			| BySym -> Smv.Var s
			| BySym' -> Smv.NextVar s
			end
		| Psl.Atom (NPr s) -> begin match s4 with
			| BySym -> Smv.Neg (Smv.Var s)
			| BySym' -> Smv.Neg (Smv.NextVar s)
			end
		| Psl.Atom (St (p, d)) -> subst_st (p, d) s0 s1 s2 s3
		| Psl.Neg bqs1 -> Smv.Neg (subst_succs bqs1 s0 s1 s2 s3 s4)
		| Psl.And (bqs1, bqs2) -> Smv.And (subst_succs bqs1 s0 s1 s2 s3 s4, subst_succs bqs2 s0 s1 s2 s3 s4)
		| Psl.Or (bqs1, bqs2) -> Smv.Or (subst_succs bqs1 s0 s1 s2 s3 s4, subst_succs bqs2 s0 s1 s2 s3 s4)
	in


	(* ========== Variables consists of all ABA states + all final ABA states *)
	(* less minterms for this order: r0 r1 ... s0 s1 ... *)
	let smv_vars : string list =
		let l = ref [] in
		for i = 0 to n - 1 do
			l := (pref^"r"^(string_of_int i)) :: !l
		done;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then l := (pref^"s"^(string_of_int i)) :: !l
		done;
		List.rev !l
	in
	(* very weak version *)
	let smv_vw_vars : string list =
		let l = ref [] in
		for i = 0 to n - 1 do
			l := (pref^"r"^(string_of_int i)) :: !l
		done;
		List.rev !l
	in
	(* ========== Symbolic representation of initial states *)
	let smv_init : Smv.varexp list =
		(* r_init /\ -s1 /\ .. /\ -sm *)
		let phi1 =
			List.fold_left
				(fun psi i -> Smv.And (psi, Smv.Neg (Smv.Var(pref^"s"^(string_of_int i)))))
				Smv.True
				nonfinalsL
		in
		let phi1 = Smv.And (Smv.Var (pref^"r"^(string_of_int aut.start)), phi1) in
		(* /\_r (r -> delta(r)[subst]) *)
		let phi2 =
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByFalse ByTrue ByRState ByTrue BySym in
					Smv.And (psi, Smv.Imp (Smv.Var (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		[phi1; phi2]
	in
	(* very weak version *)
	let smv_vw_init : Smv.varexp list =
		(* r_init /\ s!, here we represent s! by -1 *)
		let phi1 =
			if nonfinalsL = [] then Smv.Var (pref^"r"^(string_of_int aut.start))
			else Smv.And (Smv.Var (pref^"r"^(string_of_int aut.start)), Smv.Int (pref^"s", -1))
		in
		(* /\_r (r -> delta(r)[subst]) *)
		let phi2 =
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByFalse ByTrue ByRState ByTrue BySym in
					Smv.And (psi, Smv.Imp (Smv.Var (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		[phi1; phi2]
	in
	(* ========== Symbolic representation of the transition function *)
	let smv_trans : Smv.varexp list =
		let rset_forw = (* R set, forward constraints: /\_r (r -> delta(r)[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByTrue ByTrue ByRState ByRState' BySym in
					Smv.And (psi, Smv.Imp (Smv.Var (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		let rset_backw = (* R set, backward constraints: /\_r (r' -> delta(r')[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByRState ByRState ByRState' ByTrue BySym' in
					Smv.And (psi, Smv.Imp (Smv.NextVar (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		(* Case 1: S set is empty: /\_s (!s) *)
		let sset_empty =
			List.fold_left
				(fun psi i -> Smv.And (psi, Smv.Neg (Smv.Var(pref^"s"^(string_of_int i)))))
				Smv.True
				nonfinalsL
		in
		let sset_case1 = (* /\_s (r' <-> s') *)
			List.fold_left
				(fun psi i -> Smv.And (psi, Smv.Eq (Smv.NextVar (pref^"r"^(string_of_int i)), Smv.NextVar (pref^"s"^(string_of_int i)))))
				Smv.True
				nonfinalsL
		in
		let case1 = Smv.Imp (sset_empty, sset_case1) in
		(* Case 2: S set is not empty: \/_s s *)
		let sset_nonempty =
			List.fold_left
				(fun psi i -> Smv.Or (psi, Smv.Var(pref^"s"^(string_of_int i))))
				Smv.False
				nonfinalsL
		in
		let sset_forw = (* S set, forward constraints: /\_s (s -> delta(s)[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByTrue ByTrue ByFinState ByFinState' BySym in
					Smv.And (psi, Smv.Imp (Smv.Var (pref^"s"^(string_of_int i)), ri_succs)))
				Smv.True
				nonfinalsL
		in
		let sset_backw = (* S set, backward constraints: /\_s (s' -> delta(s')[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByFinState ByFinState ByFinState' ByTrue BySym' in
					Smv.And (psi, Smv.Imp (Smv.NextVar (pref^"s"^(string_of_int i)), ri_succs)))
				Smv.True
				nonfinalsL
		in
		let case2 = Smv.Imp (sset_nonempty, Smv.And (sset_forw, sset_backw)) in
		[rset_forw; rset_backw; case1; case2]
	in
	(* very weak version *)
	let smv_vw_trans : Smv.varexp list =
		let rset_forw = (* R set, forward constraints: /\_r (r -> delta(r)[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByTrue ByTrue ByRState ByRState' BySym in
					Smv.And (psi, Smv.Imp (Smv.Var (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		let rset_backw = (* R set, backward constraints: /\_r (r' -> delta(r')[subst]) *)
			List.fold_left
				(fun psi i ->
					let ri_succs = subst_succs aut.delta.(i) ByRState ByRState ByRState' ByTrue BySym' in
					Smv.And (psi, Smv.Imp (Smv.NextVar (pref^"r"^(string_of_int i)), ri_succs)))
				Smv.True
				allL
		in
		let sigma = Array.of_list (-1::nonfinalsL) in
		let nsigma = Array.length sigma in
		let sstate = (* /\_s (s -> (phi(s) -> next(s)') & (-phi(s) -> s')) *)
			let forw_succs (i:int) = subst_succs aut.delta.(i) ByTrue ByTrue ByRState ByRState' BySym in
			(* replaces next(r_i) by false *)
			let rec rem_ri (i:int) (exp:Smv.varexp) =	match exp with
				| Smv.True
				| Smv.False
				| Smv.Var _ -> exp
				| Smv.NextVar s ->
						if s=(pref^"r"^(string_of_int i)) then Smv.False else exp
				| Smv.Int _
				| Smv.NextInt _ -> exp
				| Smv.Neg e1 -> Smv.Neg (rem_ri i e1)
				| Smv.And (e1, e2) -> Smv.And (rem_ri i e1, rem_ri i e2)
				| Smv.Or (e1, e2) -> Smv.Or (rem_ri i e1, rem_ri i e2)
				| Smv.Imp (e1, e2) -> Smv.Imp (rem_ri i e1, rem_ri i e2)
				| Smv.Eq (e1, e2) -> Smv.Eq (rem_ri i e1, rem_ri i e2)
			in
			List.fold_left
				(fun psi si ->  (* si is a pointer to s_i *)
					let bigphi =
						let ex_model =
							match sigma.(si) with
								| -1 -> Smv.True
								| _ -> rem_ri sigma.(si) (forw_succs sigma.(si))
						in
						let r_p = Smv.Var (pref^"r"^(string_of_int sigma.(si))) in
						Smv.Imp (r_p, ex_model)
					in
					let impl1 = Smv.Imp (bigphi, Smv.NextInt (pref^"s", sigma.((si+1) mod nsigma))) in
					let impl2 = Smv.Imp (Smv.Neg bigphi, Smv.NextInt (pref^"s", sigma.(si))) in
					Smv.And (psi, Smv.Imp (Smv.Int (pref^"s", sigma.(si)), Smv.And(impl1, impl2))))
				Smv.True
				(nList nsigma)
		in
		if nonfinalsL = [] then [rset_forw; rset_backw]
		else [rset_forw; rset_backw; sstate]
	in
	(* ========== Symbolic representation of fair states *)
	let smv_fair : Smv.varexp list = (* S set is empty: /\_s (!s) *)
		[List.fold_left
			(fun psi i -> Smv.And (psi, Smv.Neg (Smv.Var(pref^"s"^(string_of_int i)))))
			Smv.True
			nonfinalsL]
	in

	if not(dgo) && (isVeryWeak aut) then
		let b = (nonfinalsL = []) in
		{Smv.vars = smv_vw_vars;
		Smv.ints = if b then [] else [(pref^"s", (-1 :: nonfinalsL))];
		Smv.init = smv_vw_init;
		Smv.trans = smv_vw_trans;
		Smv.fair = if b then [] else [Smv.Int (pref^"s", -1)]}
	else
		{Smv.vars = smv_vars;
		Smv.ints = [];
		Smv.init = smv_init;
		Smv.trans = smv_trans;
		Smv.fair = smv_fair}










(** Converts an very-weak 1ABA into a GNBA *)
let toSmv2 (aut:aba) (pref:string) : Smv.smv =
	(* Returns the list [0; 1; 2; ...; n-1] *)
	let nList (n:int) : int list = Array.to_list (Array.init n (fun i -> i)) in
	(* Converts an array to an association list *)
	let arr2ali (arr:'a array) = List.combine (nList (Array.length arr)) (Array.to_list arr)	in

	let n = Array.length aut.delta in
	(*let allL = nList n in *)
	(* Returns the list of final states *)
	let nonfinalsL : int list =
		let allAli = (arr2ali aut.final) in
		let finAli = List.filter (fun (_, b) -> not(b)) allAli in
		fst (List.split finAli)
	in

	(* Substitutes a state according to its direction *)
	let subst_st ((q, d):int*dir) (s0:subst) (s1:subst) (s2:subst) (s3:subst) : Smv.varexp =
		let subst2 (s:subst) : Smv.varexp = match s with
			| ByFalse -> Smv.False
			| ByTrue -> Smv.True
			| ByRState -> Smv.Var (pref^"r"^(string_of_int q))
			| BySState -> Smv.Var (pref^"s"^(string_of_int q))
			| ByRState' -> Smv.NextVar (pref^"r"^(string_of_int q))
			| BySState' -> Smv.NextVar (pref^"s"^(string_of_int q))
			| ByFinState -> begin match not(aut.final.(q)) with
				| true -> Smv.Var (pref^"s"^(string_of_int q))
				| false -> Smv.Var (pref^"r"^(string_of_int q))
				end
			| ByFinState' -> begin match not(aut.final.(q)) with
				| true -> Smv.NextVar (pref^"s"^(string_of_int q))
				| false -> Smv.NextVar (pref^"r"^(string_of_int q))
				end
		in
		match d with
			| Back -> subst2 s0
			| WBack -> subst2 s1
			| Eps -> subst2 s2
			| Forw -> subst2 s3
	in
	(* Substitutes successor states (bool. comb. of states). *)
	(* s0..s3 are the substitutions for the Back, WBack, Eps, and Forw states.*)
	(* s4 is the substitution for the propositions. *)
	let rec subst_succs (bqs:succ_states) (s0:subst) (s1:subst) (s2:subst) (s3:subst) (s4:subst') : Smv.varexp = match bqs with
		|	Psl.True -> Smv.True
		| Psl.False -> Smv.False
		| Psl.Atom (Pr s) -> begin match s4 with
			| BySym -> Smv.Var s
			| BySym' -> Smv.NextVar s
			end
		| Psl.Atom (NPr s) -> begin match s4 with
			| BySym -> Smv.Neg (Smv.Var s)
			| BySym' -> Smv.Neg (Smv.NextVar s)
			end
		| Psl.Atom (St (p, d)) -> subst_st (p, d) s0 s1 s2 s3
		| Psl.Neg bqs1 -> Smv.Neg (subst_succs bqs1 s0 s1 s2 s3 s4)
		| Psl.And (bqs1, bqs2) -> Smv.And (subst_succs bqs1 s0 s1 s2 s3 s4, subst_succs bqs2 s0 s1 s2 s3 s4)
		| Psl.Or (bqs1, bqs2) -> Smv.Or (subst_succs bqs1 s0 s1 s2 s3 s4, subst_succs bqs2 s0 s1 s2 s3 s4)
	in


	(* ========== Variables consists of all ABA states + all final ABA states *)
	(* less minterms for this order: r0 r1 ... r{n-1} *)
	let smv_vars : string list =
		List.rev
			(List.fold_left
				(fun lst i -> (pref^"r"^(string_of_int i)) :: lst)
				[]
				(nList n))
	in
	(* ========== Symbolic representation of initial states *)
	let smv_init : Smv.varexp list =
		(* r_init /\ -r1 /\ .. /\ -r{n-1} *)
		[List.fold_left
			(fun psi i ->
				if i = aut.start then
					Smv.And (psi, Smv.Var(pref^"r"^(string_of_int i)))
				else
					Smv.And (psi, Smv.Neg (Smv.Var(pref^"r"^(string_of_int i)))))
			Smv.True
			(nList n)]
	in
	(* ========== Symbolic representation of the transition function *)
	let smv_trans : Smv.varexp list =
		(* /\_r (r -> delta(r)[subst]) *)
		[List.fold_left
			(fun psi i ->
				let ri_succs = subst_succs aut.delta.(i) ByTrue ByTrue ByRState ByRState' BySym in
				Smv.And (psi, Smv.Imp (Smv.Var (pref^"r"^(string_of_int i)), ri_succs)))
			Smv.True
			(nList n)]
	in
	(* ========== Symbolic representation of fair states *)
	let smv_fair : Smv.varexp list =
		(* Fi's, where Fi := {} *)
		[(* to be done *)]
	in

	{Smv.vars = smv_vars;
	Smv.ints = [];
	Smv.init = smv_init;
	Smv.trans = smv_trans;
	Smv.fair = smv_fair}













(* ============== OLD VERSIONS -- DEPRECATED =================== *)
(* ============== OLD VERSIONS -- DEPRECATED =================== *)
(* ============== OLD VERSIONS -- DEPRECATED =================== *)




(** Converts a 2-way ABA into a nuSMV program (Dax/Klaedtke/Lange) *)
let toSmv3 (aut:aba) (tt:string) (ff:string) (pref:string) : string =
	let n = Array.length aut.delta in
	(* Substitutes a given symbol the given substitutions *)
	let subst_sym (s: string) (s1: subst') = match s1 with
		| BySym -> s
		| BySym' -> "next("^s^")"
	in
	(* Substitutes a given state according to its direction and the given    *)
	(* substitutions                                                         *)
	let subst_st ((q, d): int * dir) (s0: subst) (s1: subst) (s2: subst) (s3: subst) : string =
		let subst2 (s: subst) : string = match s with
			| ByFalse -> ff
			| ByTrue -> tt
			| ByRState -> pref^"r"^(string_of_int q)
			| BySState -> pref^"s"^(string_of_int q)
			| ByRState' -> "next("^pref^"r"^(string_of_int q)^")"
			| BySState' -> "next("^pref^"s"^(string_of_int q)^")"
			| ByFinState ->
					let t = if not(aut.final.(q)) then pref^"s" else pref^"r" in
					t^(string_of_int q)
			| ByFinState' ->
					let t = if not(aut.final.(q)) then pref^"s" else pref^"r" in
					"next("^t^(string_of_int q)^")"
		in
		match d with
		| Back -> subst2 s0
		| WBack -> subst2 s1
		| Eps -> subst2 s2
		| Forw -> subst2 s3
	in
	(* Substitutes successor states (bool. comb. of states).*)
	(* s0..s3 are the substitutions for the Back, WBack, Eps, and Forw states.*)
	(* s4 is the substitution for the propositions. *)
	let rec subst_succs (bqs: succ_states) (s0: subst) (s1: subst) (s2: subst) (s3: subst) (s4: subst') : string = match bqs with
		|	Psl.True -> tt
		| Psl.False -> ff
		| Psl.Atom (Pr s) -> subst_sym s s4
		| Psl.Atom (NPr s) -> subst_sym ("! ("^s^")") s4
		| Psl.Atom (St (p, d)) -> subst_st (p, d) s0 s1 s2 s3
		| Psl.Neg bqs1 -> "! "^(subst_succs bqs1 s0 s1 s2 s3 s4)
		| Psl.And (bqs1, bqs2) -> "("^(subst_succs bqs1 s0 s1 s2 s3 s4)^" & "^(subst_succs bqs2 s0 s1 s2 s3 s4)^")"
		| Psl.Or (bqs1, bqs2) -> "("^(subst_succs bqs1 s0 s1 s2 s3 s4)^" | "^(subst_succs bqs2 s0 s1 s2 s3 s4)^")"
	in
	(* Variable declaration: a state is a tuple (R,S) \subseteq 2^Q x        *)
	(* 2^{Q\F}                                                               *)
	let smv_states : string =
		let str = ref "" in
		for i = 0 to n - 1 do
			str := !str^"  "^pref^"r"^(string_of_int i)^" : boolean;\n";
		done;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^"  "^pref^"s"^(string_of_int i)^" : boolean;\n";
		done;
		!str
	in
	(* Symbolic representation of initial state *)
	let smv_init : string =
		let str = ref ("  "^pref^"r"^(string_of_int aut.start)^"\n ") in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" & !"^pref^"s"^(string_of_int i);
		done;
		str := !str^"\n";
		for i = 0 to n - 1 do
			str := !str^"  & ("^pref^"r"^(string_of_int i)^" -> "^
			(subst_succs aut.delta.(i) ByFalse ByTrue ByRState ByTrue BySym)^")\n";
		done;
		(!str)
	in
	(* Symbolic representation of final states *)
	let smv_finals : string =
		let str = ref ("  "^tt) in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" & !"^pref^"s"^(string_of_int i);
		done;
		!str
	in
	(* Symbolic representation of the transition function *)
	let smv_trans : string =
		let str = ref ("TRANS\n  "^tt^"\n") in
		for i = 0 to n - 1 do  (* R set, forward constraints *)
			str := !str^"  & ("^pref^"r"^(string_of_int i)^" -> "^
			(subst_succs aut.delta.(i) ByTrue ByTrue ByRState ByRState' BySym)^")\n";
		done;
		str := (!str)^"\n";
		for i = 0 to n - 1 do  (* R set, backward constraints *)
			str := !str^"  & (next("^pref^"r"^(string_of_int i)^") -> "^
			(subst_succs aut.delta.(i) ByRState ByRState ByRState' ByTrue BySym')^")\n";
		done;
		str := !str^"\n";
		(* ===== S set, if S set is empty *)
		str := (!str)^"  & (("^tt;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then
				str := !str^" & !"^pref^"s"^(string_of_int i)
		done;
		str := !str^") -> \n";
		str := !str^"      ("^tt^"\n";
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then
				str := !str^"      & (next("^pref^"r"^(string_of_int i)^") <-> next("^pref^"s"^(string_of_int i)^"))\n";
		done;
		str := (!str)^"      )\n";
		str := (!str)^"    )\n\n";
		(* =====  S set, if S set is not empty *)
		str := !str^"  & (("^ff;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then
				str := !str^"  | "^pref^"s"^(string_of_int i)
		done;
		str := !str^") ->\n";
		str := !str^"      ("^tt^"\n";  (* forward constraints ... *)
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then begin
				str := !str^"      & ("^pref^"s"^(string_of_int i)^" -> "^
				(subst_succs aut.delta.(i) ByTrue ByTrue ByFinState ByFinState' BySym)^")\n";
			end
		done;
		for i = 0 to n - 1 do  (* backward constraints ... *)
			if not(aut.final.(i)) then begin
				str := !str^"      & (next("^pref^"s"^(string_of_int i)^") -> "^
				(subst_succs aut.delta.(i) ByFinState ByFinState ByFinState' ByTrue BySym')^")\n";
			end
		done;
		str := (!str)^"      )\n";
		str := (!str)^"    )\n";
		(!str)
	in
	"-- begin of automaton\n"^
	"VAR\n"^smv_states^
	"INIT\n"^smv_init^
	smv_trans^
	"FAIRNESS\n"^smv_finals^"\n"^
	(* "SPEC\n"^ " !(State in "^(intset2str ini)^" & EG 1)\n"^ *)
	"-- end of automaton\n";;


(** Converts a 1-way ABA into a nuSMV programm (Miyano-Hayashi) *)
let toSmv_old (aut: aba) (tt:string) (ff:string) : string =
	let n = Array.length aut.delta in
	(* states as variables: a state is a tuple (R,S) \subseteq 2^Q x 2^{Q\F} *)
	let smv_states : string =
		let str = ref "" in
		for i = 0 to n - 1 do
			str := !str^"  r"^(string_of_int i)^" : boolean;\n";
		done;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^"  s"^(string_of_int i)^" : boolean;\n";
		done;
		!str
	in
	(* symbolic representation of initial state *)
	let smv_init : string =
		let str = ref "" in
		for i = 0 to n - 1 do
			if i != aut.start then str := !str^" & !r"^(string_of_int i);
		done;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" & !s"^(string_of_int i);
		done;
		"  r"^(string_of_int aut.start)^(!str)^"\n"
	in
	(* transition function *)
	let rec succs2smv (bqs: succ_states) (ren: int -> string) : string = match bqs with
		| Psl.True
		| Psl.False -> failwith "aba: True/False not used as successors."
		| Psl.Atom (Pr s) -> s
		| Psl.Atom (NPr s) -> "! "^s
		| Psl.Atom (St (q, _)) -> ren q
		| Psl.Neg b -> "! "^(succs2smv b ren)
		| Psl.And (b1, b2) -> "("^(succs2smv b1 ren)^" & "^(succs2smv b2 ren)^")"
		| Psl.Or (b1, b2) -> "("^(succs2smv b1 ren)^" | "^(succs2smv b2 ren)^")"
	in
	let smv_trans_S_empty : string =
		let str = ref ("  ("^tt) in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" & !s"^(string_of_int i)
		done;
		str := !str^") ->\n    ("^tt;
		let ren (i: int) : string = "next(r"^(string_of_int i)^")" in
		for i = 0 to n - 1 do
			str := !str^"\n    & (r"^(string_of_int i)^" -> "^(succs2smv (aut.delta.(i)) ren)^")"
		done;
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^"\n    & (next(r"^(string_of_int i)^") = next(s"^(string_of_int i)^"))"
		done;
		!str^")\n";
	in
	let smv_trans_S_notempty : string =
		let str = ref ("  ("^ff) in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" | s"^(string_of_int i)
		done;
		str := !str^") ->\n    ("^tt;
		let ren (i: int) : string = "next(r"^(string_of_int i)^")" in
		for i = 0 to n - 1 do
			str := !str^"\n    & (r"^(string_of_int i)^" -> "^(succs2smv (aut.delta.(i)) ren)^")"
		done;
		let ren2 (i: int) : string =
			if aut.final.(i) then "next(r"^(string_of_int i)^")" else "next(s"^(string_of_int i)^")"
		in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^"\n    & (s"^(string_of_int i)^" -> "^(succs2smv (aut.delta.(i)) ren2)^")"
		done;
		!str^")\n";
	in
	(* symbolic representation of final states *)
	let smv_finals : string =
		let str = ref ("  "^tt) in
		for i = 0 to n - 1 do
			if not(aut.final.(i)) then str := !str^" & !s"^(string_of_int i)
		done;
		!str
	in
	"-- begin of automaton\n"^
	"VAR\n"^smv_states^
	"INIT\n"^smv_init^
	"TRANS\n"^smv_trans_S_empty^
	"TRANS\n"^smv_trans_S_notempty^
	"FAIRNESS\n"^smv_finals^"\n"^
	(* "SPEC\n"^ " !(State in "^(intset2str ini)^" & EG 1)\n"^ *)
	"-- end of automaton\n";;
