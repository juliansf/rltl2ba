open Rltl
open Entry

let build_initial_env add_builtin mgr =
  let builtin_functions = [
    "!",
    FunEntry (fun x -> ValEntry (Expgen.bool_not mgr (val_entry x)));

    "&",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.bool_and mgr (val_entry x) (val_entry y))));

    "|",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.bool_or mgr (val_entry x) (val_entry y))));

    "->",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.bool_impl mgr (val_entry x) (val_entry y))));

    "<->",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.bool_iff mgr (val_entry x) (val_entry y))));

    "*",
    FunEntry (fun x -> ValEntry (Expgen.regex_star mgr (val_entry x)));

    "+",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.regex_plus mgr (val_entry x) (val_entry y))));

    "||",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.regex_plus mgr (val_entry x) (val_entry y))));

    "&&",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.regex_cap mgr (val_entry x) (val_entry y))));

    ".",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.regex_concat mgr (Expgen.overlap false)
                  (val_entry x) (val_entry y))));

    ";",
    FunEntry (fun x -> FunEntry (fun y ->
      let sfl = Expgen.existential() in
      let ofl = Expgen.overlap false in
      let ynode = val_entry y in
      if Expgen.is_rltl mgr ynode then
        ValEntry (Expgen.rltl_seq mgr sfl ofl (val_entry x) ynode)
      else
        ValEntry (Expgen.regex_concat mgr ofl (val_entry x) ynode)));

    ":",
    FunEntry (fun x -> FunEntry (fun y ->
      let sfl = Expgen.existential() in
      let ofl = Expgen.overlap true in
      let ynode = val_entry y in
      if Expgen.is_rltl mgr ynode then
        ValEntry (Expgen.rltl_seq mgr sfl ofl (val_entry x) ynode)
      else
        ValEntry (Expgen.regex_concat mgr ofl (val_entry x) ynode)));


    "not",
    FunEntry (fun x -> ValEntry (Expgen.rltl_not mgr (val_entry x)));

    "and",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_and mgr (val_entry x) (val_entry y))));

    "or",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_or mgr (val_entry x) (val_entry y))));

    "implies",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_impl mgr (val_entry x) (val_entry y))));

    "iff",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_iff mgr (val_entry x) (val_entry y))));

    ";;",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_seq mgr (Expgen.universal()) (Expgen.overlap false)
        (val_entry x) (val_entry y))));

    "::",
    FunEntry (fun x -> FunEntry (fun y ->
      ValEntry (Expgen.rltl_seq mgr (Expgen.universal()) (Expgen.overlap true)
        (val_entry x) (val_entry y))));

  ] in
  List.iter (fun (id,entry) -> add_builtin id entry) builtin_functions
