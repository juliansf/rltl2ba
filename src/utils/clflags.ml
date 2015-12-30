
(* Command-line parameters *)

let smvfiles = ref ([] : string list)        (* .smv files *)
and dotfiles = ref ([] : string list)        (* .dot files *)
and dot = ref false

let output_name = ref (None : string option) (* -o *)
and use_stdin = ref false                    (* -stdin *)

let verbose = ref (-1)                       (* -verbose *)
let debug = ref (-1)                         (* -debug *)
and dump_parsetree = ref false               (* -dparsetree *)
and annot = ref false                        (* -annot *)
and dump_intcode = ref false                 (* -dintcode *)

let nfa = ref false                          (* -nfa *)
let ahw = ref false                          (* -ahw *)
let nbw = ref false                          (* -nbw *)
let psl = ref false                          (* -psl *)

let ranking = ref "stratified"               (* -kind *)
