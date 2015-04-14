
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

let get_outfmt ppf =
  match !Clflags.output_name with
  | None -> ppf
  | Some name ->
    if name = "stdout" then Format.std_formatter
    else Format.formatter_of_out_channel (open_out name)

let expression_file expected_type ppf sourcefile =
  Location.input_name := sourcefile;
  let outfmt = get_outfmt ppf in
  let tyenv = Typeenv.initial in
  let printaut = ref (!Clflags.nfa || !Clflags.ahw || !Clflags.nbw) in
  try
    _file ppf Parse.expression sourcefile
    ++ print_if ppf Clflags.dump_parsetree Printast.print_expr
    ++ Typecheck.expression tyenv
    ++ print_if ppf Clflags.annot
      (fun ppf exp -> Printtyp.print_type ppf exp.Typedtree.texp_type)
    ++ Translate.expression expected_type
    ++ print_if ppf Clflags.dump_intcode Translate.print_expr
    ++ Translate.automata
    ++ print_if outfmt printaut Translate.print_automata
    ++ (fun _ -> ())
  with e ->
    raise e

let expression_string expected_type ppf str =
  Location.input_name := "(string)";
  let outfmt = get_outfmt ppf in
  let tyenv = Typeenv.initial in
  let printaut = ref (!Clflags.nfa || !Clflags.ahw || !Clflags.nbw) in
  try
    _string ppf Parse.expression str
    ++ print_if ppf Clflags.dump_parsetree Printast.print_expr
    ++ Typecheck.expression tyenv
    ++ print_if ppf Clflags.annot
      (fun ppf exp -> Printtyp.print_type ppf exp.Typedtree.texp_type)
    ++ Translate.expression expected_type
    ++ print_if ppf Clflags.dump_intcode Translate.print_expr
    ++ Translate.automata
    ++ print_if outfmt printaut Translate.print_automata
    ++ (fun _ -> ())
  with e ->
    raise e
