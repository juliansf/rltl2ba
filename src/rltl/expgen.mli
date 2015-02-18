
open Exptypes
open Exptree

type manager
type node

exception Undefined_node of node
exception Node_type_clash of node
exception Non_overlapping_node of node
exception Node_already_overlapped of node

val node_id: node -> int

val init: unit -> manager
val new_var: manager -> string -> node
val const_true: node
val const_false: node

val bool_not: manager -> node -> node
val bool_and: manager -> node -> node -> node
val bool_or: manager -> node -> node -> node
val bool_impl: manager -> node -> node -> node
val bool_iff: manager -> node -> node -> node

val regex_star: manager -> node -> node
val regex_plus: manager -> node -> node -> node
val regex_cap: manager -> node -> node -> node
val regex_concat: manager -> overlap_flag -> node -> node -> node

val rltl_not: manager -> node -> node
val rltl_or: manager -> node -> node -> node
val rltl_and: manager -> node -> node -> node
val rltl_impl: manager -> node -> node -> node
val rltl_iff: manager -> node -> node -> node
val rltl_seq: manager -> seq_flag -> overlap_flag -> node -> node -> node
val rltl_power
  : manager -> power_flag -> overlap_flag -> node -> node -> node -> node
val rltl_closure: manager -> node -> node

val mk_overlap: manager -> node -> node

(* Node type tests *)
val is_bool: manager -> node -> bool
val is_regex: manager -> node -> bool
val is_rltl: manager -> node -> bool

(* Flags for Overlapping, Sequential and Power Operators *)
val overlap: bool -> overlap_flag

val existential: unit -> seq_flag
val universal: unit -> seq_flag

val regular: unit -> power_flag
val weak: unit -> power_flag
val dual: unit -> power_flag
val dualweak: unit -> power_flag

val positive: unit -> closure_flag
val negative: unit -> closure_flag

(* Pretty printer of node tree *)
val print_node: manager -> Format.formatter -> node -> unit
val print_manager: Format.formatter -> manager -> unit
