(** Routines to transform ABAs to NBAs *)

type bexp = Psl.bexp
type sere = Psl.sere
type psl = Psl.psl


(** Defines the type of an NBA. The transition function is defined as delta: states --> 2^(bexp x states) *)
type transition = (bexp * int) list
type nba = {
	delta : transition array;
	start : int;
	final : bool array;
}


(** Interprets an NFA as an NBA *)
let fromNfa (aut1:Nfa.nfa) : nba =
	{delta=aut1.Nfa.delta; start=aut1.Nfa.start; final=aut1.Nfa.final;}

(** Dax/Lange/Klaedtke construction *)
let fromAba (aut:Aba.aba) : string =
	"-- from Aba"
	
	

 