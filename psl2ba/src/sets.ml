(** Some functions to handle sets *)

(** Converts a list into a set *)
let fromList (lst:'a list) : 'a list =
	let rec rem_duplicates (l: 'a list) : 'a list = match l with
		| [] -> []
		| h::[] -> [h]
		| h1::h2::tl -> 
				if h1 = h2 then (rem_duplicates (h2::tl)) else h1::(rem_duplicates (h2::tl))
	in   
	rem_duplicates (List.sort compare lst)


module Int = struct
	type t = int
	let compare = compare
end
module IntSet = Set.Make(Int);;
module IntMap = Map.Make(Int);;

module Str = struct
	type t = string
	let compare = compare
end
module StrSet = Set.Make(Str);;
module StrMap = Map.Make(Str);;

(* Successors of an NFA *)
module Succ = struct
	type t = (Psl.bexp * int) list
	let compare = compare
end
module SuccSet = Set.Make(Succ);;
module SuccMap = Map.Make(Succ);;


module Int2 = struct
	type t = int * int
	let compare = compare
end
module Int2Set = Set.Make(Int2);;
module Int2Map = Map.Make(Int2);;

(* delta relation of an automaton *)
module Int3 = struct
	type t = int * int * int
	let compare = compare
end
module Int3Set = Set.Make(Int3);;

(* delta relation of a product automaton *)
module Int5 = struct
	type t = (int*int) * int * (int * int)
	let compare = compare
end
module Int5Set = Set.Make(Int5);;



(*
(* successors of a state: delta: Q --> 2^{\calB(S) x Q} *)
module Trans = struct
	type t = bexp * int
	let compare = compare
end
module TransSet = Set.Make(Trans);;

(* interpretations or possible models for the gates *)
module Model = struct
	type t = GateSet.t
	let compare = GateSet.compare
end
module ModelSet = Set.Make(Model);;
module ModelMap = Map.Make(Model);;


(* for optimizing the never claim output: (q',model)  set *)
module Modq = struct
	type t = string * int
	let compare = compare
end
module ModqSet = Set.Make(Modq);;
*)