
open Config
open Clflags

let process_expression_file ppf name =
  Compile.expression_file ppf name

let process_expression_string ppf s =
  Compile.expression_string ppf s

let process_file ppf name =
  if Filename.check_suffix name !Config.rltl_suffix then begin
    process_expression_file ppf name
  end
  else
    raise (Arg.Bad("don't know what to d with " ^ name))

let print_version_string () =
  print_string Config.version; print_newline (); exit 0

let usage = "Usage: rltl2ba <options> <files>\nOptions are:"

let ppf = Format.err_formatter

let anon = process_file ppf
let expr = process_expression_file ppf
let str = process_expression_string ppf

let show_config () =
  Config.print_config stdout;
  exit 0

module Options = Main_args.Make_rltlba_options(struct
  let set r () = r := true
  let unset r () = r := false
  let _annot = set annot
  let _dintcode = set dump_intcode
  let _dparsetree = set dump_parsetree
  let _i = anon
  let _o s = output_name := Some s
  let _s = str
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
  with e ->
    Errors.report_error ppf e;
    exit 2

let _ = main ()
