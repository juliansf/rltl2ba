
open Config
open Clflags

let process_expression_file ty ppf name =
  Compile.expression_file ty ppf name

let process_expression_string ty ppf s =
  Compile.expression_string ty ppf s

let process_file ppf name =
  if Filename.check_suffix name !Config.rltl_suffix then
    process_expression_file Predef.type_rltl ppf name
  else if Filename.check_suffix name !Config.reg_suffix then
    process_expression_file Predef.type_regex ppf name
  else if Filename.check_suffix name !Config.psl_suffix || !Clflags.psl then
    begin
      Clflags.psl := true;
      process_expression_file Predef.type_rltl ppf name
    end
  else
    raise (Arg.Bad("don't know what to d with " ^ name))

let process_string ppf s =
  if !Clflags.nfa then
    process_expression_string Predef.type_regex ppf s
  else if !Clflags.ahw then
    if !Clflags.psl then
      process_expression_string Predef.type_rltl ppf s
    else
      process_expression_string Predef.type_rltl ppf s
  else
    process_expression_string Predef.type_rltl ppf s (* default *)

let print_version_string () =
  print_string Config.version; print_newline (); exit 0

let usage = "Usage: rltl2ba <options> <files>\nOptions are:"

let ppf = Format.err_formatter

let anon = process_file ppf
let expr = process_expression_file Predef.type_rltl ppf
let str = process_string ppf

let show_config () =
  Config.print_config stdout;
  exit 0

module Options = Main_args.Make_rltlba_options(struct
  let set r () = r := true
  let unset r () = r := false
  let set_kind (a,b,c) ()=
    if !b || !c then raise (Arg.Bad "Options -nfa, -apw and -nbw are incompatible")
    else a := true
  let _annot = set annot
  let _ahw = set_kind (ahw,nfa,nbw)
  let _dintcode = set dump_intcode
  let _dparsetree = set dump_parsetree
  let _dot = set dot
  let _i = anon
  let _nbw = set_kind (nbw,nfa,ahw)
  let _nfa = set_kind (nfa,ahw,nbw)
  let _o s = output_name := Some s
  let _psl = (fun _ -> Printf.fprintf stderr "setting psl...\n"; set psl ())
  let _rank s =
    match s with
    | "full" | "max2" | "stratified" -> ranking := s
    | _ -> raise (Arg.Bad ("Unknown ranking '" ^ s ^ "'"))
  let _s = (fun s -> str s)
(*  let _stdin () = set use_stdin *)
  let _verbose n = verbose := n
  let _version = print_version_string
end)

let fatal err =
  prerr_endline err;
  exit 2

let main () =
  try
    Arg.parse Options.list anon usage;
    (*Gc.print_stat stderr;*)
    Printf.eprintf "Memory used: %s\n\n"
      (Misc.human_readable_byte_count (Gc.allocated_bytes ()));
    exit 0
  with e ->
    Errors.report_error ppf e;
    exit 2

let _ = main ()
