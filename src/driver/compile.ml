
let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let _file ppf parse_f inputfile =
  let ic = open_in_bin inputfile in
  let ast =
    try
      seek_in ic 0;
      Location.input_name := inputfile;
      let lexbuf = Lexing.from_channel ic in
      Location.init lexbuf inputfile;
      parse_f lexbuf
    with e -> close_in ic; raise e
  in
  close_in ic;
  ast

let _string ppf parse_f str =
  let fname = "(string)" in
  Location.input_name := fname;
  let ast =
    try
      let lexbuf = Lexing.from_string str in
      Location.init lexbuf fname;
      parse_f lexbuf
    with e -> raise e
  in
  ast

let expression_file ppf sourcefile =
  Location.input_name := sourcefile;
  let tyenv = Typeenv.initial in
  try
    _file ppf Parse.expression sourcefile
    ++ print_if ppf Clflags.dump_parsetree Printast.print_expr
    ++ Typecheck.expression tyenv
    ++ print_if ppf Clflags.annot
      (fun ppf exp -> Printtyp.print_type ppf exp.Typedtree.texp_type)
    ++ Translate.expression Predef.type_rltl
    ++ print_if ppf Clflags.dump_intcode Translate.print_expr
    ++ (fun _ -> ())
  with e ->
    raise e

let expression_string ppf str =
  Location.input_name := "(string)";
  let tyenv = Typeenv.initial in
  try
    _string ppf Parse.expression str
    ++ print_if ppf Clflags.dump_parsetree Printast.print_expr
    ++ Typecheck.expression tyenv
    ++ print_if ppf Clflags.annot
      (fun ppf exp -> Printtyp.print_type ppf exp.Typedtree.texp_type)
    ++ Translate.expression Predef.type_rltl
    ++ print_if ppf Clflags.dump_intcode Translate.print_expr
    ++ (fun _ -> ())
  with e ->
    raise e
