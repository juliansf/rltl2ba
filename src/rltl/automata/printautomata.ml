open Autmanager

let print_manager fmt mgr =
  Printbdd.print_vars fmt mgr.aut_bddmgr

let print_nfa mgr fmt nfa : unit =
(*  Printnfa.fprintf
    (Printnfa.raw_desc (Printbdd.string_of_node mgr.aut_bddmgr)) fmt nfa*)
  Printnfa.print_nfa (Printbdd.print_node mgr.aut_bddmgr) fmt nfa

let nfa2dot mgr fmt nfa : unit =
  Printnfa.fprintf
    (Printnfa.dot_desc (Printbdd.string_of_node mgr.aut_bddmgr)) fmt nfa
