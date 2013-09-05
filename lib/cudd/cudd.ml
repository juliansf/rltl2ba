(** Functions for BDD transformations. The names of the functions
		are similar to the names of the CUDD library. However, we sometimes
		changed the number of arguments.
*)

external hello_init: unit -> int = "hello_init"
external xcall_caml: (int -> unit) -> (int array -> unit) -> unit = "call_caml"

type manager
type node
external _Init : unit -> manager = "cCudd_Init"
external _Quit : manager -> unit = "Cudd_Quit"
external _bddNewVar : manager -> node = "cCudd_bddNewVar"
(*
external _bddIthVar : manager -> int -> node = "Cudd_bddIthVar"
external _ReadSize : manager -> int = "Cudd_ReadSize"
external _PrintMinterm : manager -> node -> unit = "Cudd_PrintMinterm"
*)

external _ReadOne : manager -> node = "cCudd_ReadOne"
external _ReadLogicZero : manager -> node = "cCudd_ReadLogicZero"
external _bddAnd : manager -> node -> node -> node = "cCudd_bddAnd"
external _bddOr : manager -> node -> node -> node = "cCudd_bddOr"
external _Not : node -> node = "cCudd_Not"

external _Ref : node -> unit = "cCudd_Ref"
external _RecursiveDeref : manager -> node -> unit = "cCudd_RecursiveDeref"
external _DagSize : node -> int = "cCudd_DagSize"

external _ForeachCube : (int array -> unit) -> manager -> node -> unit = "cCudd_ForeachCube"
external _ForeachPrime : (int array -> unit) -> manager -> node -> unit = "cCudd_ForeachPrime"

(*
(* Stubs that have different parameters *)
(*external xCudd_DumpDot : manager -> node -> string -> unit = "cCudd_DumpDot"*)
*)
