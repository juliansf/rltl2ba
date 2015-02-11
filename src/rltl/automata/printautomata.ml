open Autmanager

module Ahw = Ahw.Make(Nfa.Make(Bool.Default.B))

let print_manager fmt mgr =
  Printbdd.print_vars fmt mgr.aut_bddmgr

let print_nfa mgr fmt nfa : unit =
(*  Printnfa.fprintf
    (Printnfa.raw_desc (Printbdd.string_of_node mgr.aut_bddmgr)) fmt nfa*)
  Printnfa.print_nfa
    (fun fmt l -> Format.fprintf fmt "%s" (Bool.Default.B.to_string l)) fmt nfa

let nfa2dot mgr fmt nfa : unit =
  Printnfa.fprintf (Printnfa.dot_desc (Bool.Default.B.to_string)) fmt nfa

let print_ahw mgr fmt ahw : unit =
  Printahw.print_ahw mgr.aut_ahwmgr fmt ahw.Ahw.ahw_regular

let ahw2dot mgr fmt ahw : unit =
  Printahw.ahw2dot mgr.aut_ahwmgr fmt ahw.Ahw.ahw_regular

(*
let print_nfa mgr fmt nfa : unit =
(*  Printnfa.fprintf
    (Printnfa.raw_desc (Printbdd.string_of_node mgr.aut_bddmgr)) fmt nfa*)
  Printnfa.print_nfa (Printbdd.print_node mgr.aut_bddmgr) fmt nfa

let nfa2dot mgr fmt nfa : unit =
  Printnfa.fprintf
    (Printnfa.dot_desc (Printbdd.string_of_node mgr.aut_bddmgr)) fmt nfa

let print_abw mgr fmt abw : unit =
  Printabw.print_abw mgr.aut_abwmgr fmt abw.Abw.abw_regular

let abw2dot mgr fmt abw : unit =
  Printabw.fprintf mgr.aut_abwmgr fmt abw.Abw.abw_regular
*)
