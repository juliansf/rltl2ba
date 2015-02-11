
let wrap parsing_f lexbuf =
  try
    Lexer.init();
    let ast = parsing_f Lexer.token lexbuf in
    ast
  with
  | Parser.Error ->
    let loc = Location.curr lexbuf in
    raise (Syntaxerr.Error(Syntaxerr.Other loc))

let psl_wrap parsing_f lexbuf =
  try
    Psl_lexer.init();
    let ast = parsing_f Psl_lexer.token lexbuf in
    ast
  with
  | Psl_parser.Error ->
    let loc = Location.curr lexbuf in
    raise (Syntaxerr.Error(Syntaxerr.Other loc))

let expression lexbuf =
    if !Clflags.psl then begin
      Printf.fprintf stderr "PSL Parser...\n";
      psl_wrap Psl_parser.expression lexbuf
    end
    else begin
      Printf.fprintf stderr "RLTL Parser...\n";
      wrap Parser.expression lexbuf
    end
