(** Routines to manipulate graphs *)

(** A graph is defined by a successor map. 
		IMPORTANT: Is is assumed that the int list is actually a set *)
type graph = (int list) array


(** Computes the SCCs of a graph according to Tarjans algorithm. 
		Returns an array that maps each vertex to its SCC. Note that the highest
		number of an SCC name does NOT coincide with the number of SCCs.  
		
		index = 0                       // DFS node number counter 
		S = empty                       // An empty stack of nodes
		tarjan(v0)                      // Start a DFS at the start node
		
		procedure tarjan(v)
		  v.index = index               // Set the depth index for v
		  v.lowlink = index
		  index = index + 1
		  S.push(v)                     // Push v on the stack
		  forall (v, v') in E do        // Consider successors of v 
		    if (v'.index is undefined)  // Was successor v' visited? 
		      tarjan(v')                // Recurse
		      v.lowlink = min(v.lowlink, v'.lowlink)
		    elseif (v' in S)            // Is v' on the stack?
		      v.lowlink = min(v.lowlink, v'.index)
		  if (v.lowlink == v.index)     // Is v the root of an SCC?
		    print "SCC:"
		    repeat
		      v' = S.pop
		      print v'
		    until (v' == v)
*)

let sccs (succ:graph) : int array =
	let n = Array.length succ in
	let vindex = Array.create n (-1) in
	let vlowlink = Array.create n (-1) in
	let index = ref 0 in
	let stack = ref [] in
	
	let rec tarjan (v:int) : unit =
		vindex.(v) <- !index;
		vlowlink.(v) <- !index;
		index := !index + 1;
		stack := v :: !stack;

		let process_succs (v':int) : unit =
			if vindex.(v') = (-1) then begin  
				tarjan(v'); 
				vlowlink.(v) <- min vlowlink.(v) vlowlink.(v')
			end else if List.mem v' !stack then
				vlowlink.(v) <- min vlowlink.(v) vlowlink.(v');
		in
		List.iter process_succs succ.(v);
		
		if vlowlink.(v) = vindex.(v) then begin  (* v is root of SCC *)
		  (* print_endline "SCC:"; *)
			let rec pops (st:int list) : int list = match st with
				| [] -> []
				| hd::tl -> 
					(*print_endline ("  "^(string_of_int hd));*) 
					if hd=v then tl else pops tl
			in 
			stack := pops !stack;  (* pop until poped vertex is v *)
		end
	in
	for v=0 to n-1 do
		if vindex.(v) = (-1) then tarjan(v)
	done;
	vlowlink
	