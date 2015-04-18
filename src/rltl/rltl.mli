open Exptypes
open Exptree
open Shared

module Expgen : sig
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
  val bool_xor: manager -> node -> node -> node
  val bool_impl: manager -> node -> node -> node
  val bool_iff: manager -> node -> node -> node

  val regex_star: manager -> node -> node
  val regex_plus: manager -> node -> node -> node
  val regex_cap: manager -> node -> node -> node
  val regex_concat: manager -> overlap_flag -> node -> node -> node

  val rltl_not: manager -> node -> node
  val rltl_or: manager -> node -> node -> node
  val rltl_and: manager -> node -> node -> node
  val rltl_xor: manager -> node -> node -> node
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

  (* Pretty printer of node tree *)
  val print_node: manager -> Format.formatter -> node -> unit
  val print_manager: Format.formatter -> manager -> unit
end

module Automata : sig
  type t
  type label
  type nfa
  type ahw
  type nbw
  type automata =
  | Nbw of nbw
  | Ahw of ahw
  | Nfa of nfa

  val fullRank : ranking
  val maxTwoRank : ranking
  val stratifiedRank : ranking

  val init: Expgen.manager -> t
  val get_label: t -> Expgen.node -> label
  val get_nfa: t -> Expgen.node -> nfa
  val get_ahw: ?simpl:bool -> t -> Expgen.node -> ahw
  val get_nbw: ?rank:ranking -> t -> Expgen.node -> nbw

  (* Pretty printers *)
  val print_manager: Format.formatter -> t -> unit
  val print_nfa: t -> Format.formatter -> nfa -> unit
  val nfa2dot: t -> Format.formatter -> nfa -> unit
  val print_ahw: t -> Format.formatter -> ahw -> unit
  val ahw2dot: t -> Format.formatter -> ahw -> unit
  val print_nbw: t -> Format.formatter -> nbw -> unit
  val nbw2dot: t -> Format.formatter -> nbw -> unit
end
