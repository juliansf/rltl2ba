(** Routines to transform nondeterministic finite automata (NFAs) *)

(*	An NFA consists of states Q, 
	an alphabet S=2^P union {true, false}, where P is the set of propositions,
	a transition function: Q --> 2^{\calB(P) x Q}, and
	an acceptance set F \subseteq Q.
	See http://www.tcs.ifi.lmu.de/lehre/da_fopra/Marie-Fleur_Revel.pdf for more detail. *)

 

open Sets


type bexp = Psl.bexp
type sere = Psl.sere
type psl = Psl.psl


(** Defines the type of an NFA. The transition function is defined as delta: states --> 2^(bexp x states) *)
(* Note: if we used NFAs with several initial states, we could improve *)
(* the union of NFAs, but on the other hand, concatenation of NFAs would *)
(* become more complicated *)
type transition = (bexp * int) list
type nfa = {
	delta : transition array;
	start : int;
	final : bool array;
}


(** Returns the number of states of an NFA *)
let size (nfa:nfa) : int = Array.length nfa.delta;;  

(** Renumbers the states of a transition graph of an NFA by increasing the states by k.
		Side effect: this procedure changes the transition graph. *)
let incr_states_by (k:int) (delta:transition array) =
	for i=0 to (Array.length delta)-1 do
		delta.(i) <- List.map (fun (g,i) -> (g,i+k)) delta.(i);
	done;;



(** Converts an NFA into a string *)
let toStr (nfa:nfa) : string =
	(* Converts a boolean expression into a string *)
	let rec bexp2str (b:bexp) : string = match b with
		| Psl.True -> "tt"
		| Psl.False -> "ff"
		| Psl.Atom s -> s
		| Psl.Neg c -> "! "^(bexp2str c)
		| Psl.And (c, d) -> "("^(bexp2str c)^" & "^(bexp2str d)^")"
		| Psl.Or (c, d) -> "("^(bexp2str c)^" | "^(bexp2str d)^")"
	in
	(* Converts the transitions to a string *)
	let tr2str (i:int) (t:transition) : string =
		"  "^(string_of_int i)^
		(if i=nfa.start then 
			(if nfa.final.(i) then " if" else " i ") 
		else 
			(if nfa.final.(i) then "  f" else "   "))^
		"\t: "^
		(List.fold_left
			(fun s (g,q) -> s^"["^(bexp2str g)^", "^(string_of_int q)^"], ")
			""
			t)^
		"\n" 
	in
	"NFA:\n"^
	(Array.fold_left (fun s a -> s^a) "" (Array.mapi tr2str nfa.delta))
















(** Removes unreachable states. *)
let rem_unreachables (aut:nfa) : nfa = 
	let n = Array.length aut.delta in
	let reachable = Array.make n false in
	(* Mark reachable states *)
	reachable.(aut.start) <- true;
	Array.iter 
		(fun qs -> 
			List.iter (fun (b, q) -> reachable.(q) <- true) qs) 
		aut.delta;
	(* Calculate how much each state gets decreased = number of empty slots before q *)
	let dec = Array.make n 0 in
	for i=1 to n-1 do
		let inc = if not(reachable.(i-1)) then 1 else 0 in
		dec.(i) <- dec.(i-1) + inc
	done;
	(* Construct new NFA *)
	let n' = Array.fold_left (fun i b -> if b then i+1 else i) 0 reachable in  (* Count reachable states *)
	let del = Array.make n' [] in
	let fin = Array.make n' false in
	for i=0 to n-1 do
		if reachable.(i) then begin
			(* rename states and store them *)
			del.(i-dec.(i)) <- List.map (fun (b, q) -> (b, q-dec.(q))) aut.delta.(i);
			fin.(i-dec.(i)) <- aut.final.(i)
		end	
	done;
	{delta=del; start=aut.start-dec.(aut.start); final=fin}


(** Merges states that (i) have equal successor sets and 
    (i) are both, either accepting or rejecting *)
let merge_states (aut:nfa) : nfa =
	(* Merges nonfinal states that have equal successor sets *)
	let merge (aut:nfa) (bfin:bool) : nfa =
		let s2q = ref SuccMap.empty in  (* succlist -> min_state *)
		let q2q = ref IntMap.empty in  (* redirect: state -> min_state *)
		(* store least state of the set of states that direct to same proposition *)
		let redirect (q:int) (qs:transition) : unit =
			if aut.final.(q)=bfin then begin  (* redirect only final/nonfinal states *)
				if not(SuccMap.mem qs !s2q) then
					s2q := SuccMap.add qs q !s2q
				else
					let minq = SuccMap.find qs !s2q in
					q2q := IntMap.add q minq !q2q
			end				
		in
		redirect aut.start aut.delta.(aut.start);  (* prefer init state since it cannot be removed *)
		Array.iteri	redirect aut.delta;
		(* redirect states *)
		let rename (qs:transition) : transition = 
			List.map
				(fun (b, q) -> 
					if IntMap.mem q !q2q then 
						(b, IntMap.find q !q2q)
					else (b, q))
				qs
		in
		let del = Array.map rename aut.delta in
		{delta=del; start=aut.start; final=aut.final}
	in
	let aut = merge aut true in (* merge final states *)
	let aut = merge aut false in (* merge nonfinal states *)
	aut
	

(** Calls merge_states and then rem_unreachables *)
let simplify (aut:nfa) : nfa =
	rem_unreachables (merge_states aut)





(** Translates a boolean expression to an NFA *)
let fromBexp (b:bexp) : nfa =
	let delta = Array.make 2 [] in
	let final = Array.make 2 false in
	delta.(0) <- [(b,1)];
	final.(1) <- true;
	{delta=delta; start=0; final=final};;


(** Concatenates two NFAs ';' *)
let concat (nfa1:nfa) (nfa2:nfa) : nfa =
	let m1 = size nfa1 in

	incr_states_by m1 nfa2.delta;  (* renumber nfa2 *)
	let delta = Array.append nfa1.delta nfa2.delta in
	(* final states of nfa1 linked to successors of inital state of nfa2 *)
	for i=0 to m1-1 do
		if nfa1.final.(i) then
				delta.(i) <- (List.rev_append delta.(i) delta.(nfa2.start+m1));
	done;
	let final =
		if nfa2.final.(nfa2.start) then Array.append nfa1.final nfa2.final
		else Array.append (Array.make m1 false) nfa2.final
	in
	simplify {delta=delta; start=nfa1.start; final=final};;


(** Concatenates two NFAs with overlapping letter ':' *)
let fusion (nfa1:nfa) (nfa2:nfa) : nfa =
	let m1 = size nfa1 in

	incr_states_by m1 nfa2.delta;  (* renumber nfa2 *)
	let delta = Array.append nfa1.delta nfa2.delta in
	
	(* for edges that end in final states of nfa1, *)
	(* add edges to successors of inital state of nfa2 *)
	for q=0 to m1-1 do                  (* foreach q in Q1 *)
		List.iter	( fun (g, q') ->        (* forach (g,q') in delta1(q) *)
			if nfa1.final.(q') then
				List.iter	( fun (g', i') ->   (* foreach (g',i') in delta2(ini2) *)      
					delta.(q) <- (Psl.And (g, g'), i') :: delta.(q)
				)	nfa2.delta.(nfa2.start)
		) nfa1.delta.(q)
	done;

	let final =	Array.append (Array.make m1 false) nfa2.final in
	simplify {delta=delta; start=nfa1.start; final=final};;



(** constructs the union of two NFAs *)
let union (nfa1:nfa) (nfa2:nfa) : nfa =
	let m1 = size nfa1 in
	let m2 = size nfa2 in

	incr_states_by m1 nfa2.delta;  (* renumber nfa2 *)
	(* new starting state: |aut'| = m1 + m2 + 1 *)
	let delta1 = Array.make 1 (List.rev_append nfa1.delta.(nfa1.start) nfa2.delta.(nfa2.start)) in
	let final1 = Array.make 1 (nfa1.final.(nfa1.start) || nfa2.final.(nfa2.start)) in

	let delta = Array.append (Array.append nfa1.delta nfa2.delta) delta1 in
	let final = Array.append (Array.append nfa1.final nfa2.final) final1 in
	simplify {delta=delta; start=m1+m2; final=final;}



(** Constructs the product of two NFAs *)
let product (nfa1:nfa) (nfa2:nfa) : nfa =
	let m1 = size nfa1 in
	let m2 = size nfa2 in
	(* merge gates if two pairs have equal states *)
	let rec merge_states (trans:transition) : transition = match trans with
		| [] -> []
		| (g, q)::_ ->
			let (tr1, tr2) = List.partition (fun (_, q') -> q'=q) trans in
			let union_gs = List.fold_left 
				(fun gs (g', _) -> Psl.Or (gs, g')) 
				(fst (List.hd tr1)) 
				(List.tl tr1) 
			in   
			(union_gs, q) :: (merge_states tr2) 
	in
	(* product of transitions *)
	let make_product (l1:transition) (l2:transition) : (bexp*(int*int)) list =
		let a1 = Array.of_list (merge_states l1) in
		let a2 = Array.of_list (merge_states l2) in
		let m1 = Array.length a1 in
		let m2 = Array.length a2 in
		let l = ref [] in
		for i=0 to m1-1 do
			for j=0 to m2-1 do
				let g1 = fst a1.(i) in
				let q1 = snd a1.(i) in
				let g2 = fst a2.(j) in
				let q2 = snd a2.(j) in
				l := ((Psl.And (g1, g2)), (q1, q2)) :: !l;
			done
		done;
		!l
	in
	(* depth-first search for reachable states *)
	let visited = Array.make_matrix m1 m2 (-1) in
	let count = ref 0 in
	let delta = ref [] in
	
	let rec dfs (q1:int) (q2:int) =
		(* tr contains all edges from state (q1,q2) *)
		let tr = make_product nfa1.delta.(q1) nfa2.delta.(q2) in
		delta := tr :: !delta;        (* = [[succs of q_n]; [succs of q_{n-1}]; ...; [succs of q_0] *)
		visited.(q1).(q2) <- !count;  (* each pair gets a unique number *)
		count := !count + 1;
	
		let succs = Array.of_list (List.map snd tr) in
		for i=0 to (Array.length succs)-1 do
			let q1' = fst succs.(i) in
			let q2' = snd succs.(i) in
			if visited.(q1').(q2')<0 then dfs q1' q2';
		done;
	in
	dfs nfa1.start nfa2.start;
	let arrD = Array.of_list (List.rev !delta) in
	let m = Array.length arrD in
	let delta' = Array.make m [] in
	for i=0 to (Array.length arrD)-1 do  (* rename pairs by new numbers *)
		let tr = List.map (fun (g,(q1',q2')) -> (g, visited.(q1').(q2'))) arrD.(i) in
		delta'.(i) <- tr;
	done;
	(* defining the acceptance set *)
	let final = Array.make m false in
	for i=0 to m1-1 do
		for j=0 to m2-1 do
			let k = visited.(i).(j) in
			if k>=0 then final.(k) <- nfa1.final.(i) && nfa2.final.(j);
		done
	done;
	simplify {delta=delta'; start=0; final=final;};;



(** Constructs the Kleen star NFA of an NFA *)
let kleene (nfa:nfa) : nfa =
	let m = size nfa in
	(* new initial state *)
	let delta1 = Array.make 1 nfa.delta.(nfa.start) in
	let final1 = Array.make 1 true in

	(* from final states also to the successors of the initial state *)
	for i=0 to m-1 do
	if nfa.final.(i) then
		nfa.delta.(i) <- (List.rev_append nfa.delta.(i) nfa.delta.(nfa.start));
	done;
	let delta = Array.append nfa.delta delta1 in
	let final = Array.append nfa.final final1 in
	simplify {delta=delta; start=m; final=final;}


(** Constructs the mirror NFA of an NFA *)
let mirror (aut:nfa) : nfa =
	let n = Array.length aut.delta in
	(* number of final states *)
	let nFin = Array.fold_left (fun i b -> if b then i+1 else i) 0 aut.final in  
	match nFin with 
		| 0 -> fromBexp Psl.False;
		| 1 -> 
				let init' = ref 0 in
				for i=0 to n-1 do  (* find uniqe final state *)
					if aut.final.(i) then init' := i
				done;
				let delta' = Array.make n [] in 
				for i=0 to n-1 do
					List.iter 
						(fun (gate, q) -> 
								delta'.(q) <- (gate, i) :: delta'.(q))
						aut.delta.(i)
				done;
				(* final state in aut' *)
				let final' = Array.make n false in
				final'.(aut.start) <- true;
				simplify {delta=delta'; start=(!init'); final=final'}
		| _ ->
				let delta' = Array.make (n+1) [] in  (* n is new initial state in aut' *)
				for i=0 to n-1 do
					List.iter 
						(fun (gate, q) -> 
								delta'.(q) <- (gate, i) :: delta'.(q);
								if aut.final.(q) then delta'.(n) <- (gate, i) :: delta'.(n))
						aut.delta.(i)
				done;
				(* final state in aut' *)
				let final' = Array.make (n+1) false in
				final'.(aut.start) <- true;
				if aut.final.(aut.start) then final'.(n) <- true;  (* if epsilon word is accepted *) 
				simplify {delta=delta'; start=n; final=final'}
		

(** Translates a SERE into an NFA *)
let rec fromSere (reg:sere) : nfa = match reg with
	Psl.BExp b -> fromBexp b
	| Psl.Star r -> kleene (fromSere r)
	| Psl.Col (r1,r2) -> fusion (fromSere r1) (fromSere r2)
	| Psl.Conc (r1,r2) -> concat (fromSere r1) (fromSere r2)
	| Psl.Cup (r1,r2) -> union (fromSere r1) (fromSere r2)
	| Psl.Cap (r1,r2) -> product (fromSere r1) (fromSere r2);;


(** Translates a SERE into an NBA that accepts the closure of the SERE *)
let toNba (reg:sere) : nfa =
	failwith "The CL operator will be implemented soon."
	(*******************************************
		has to be implemented! 
	********************************************)
	(*(fromSere reg)*)

(** Translates a SERE into an NBA that accepts the negated closure of the SERE *)
let toNegNba (reg:sere) : nfa =
	failwith "The CL operator will be implemented soon."
	(*******************************************
		has to be implemented! 
	********************************************)
	(*(fromSere reg)*)





