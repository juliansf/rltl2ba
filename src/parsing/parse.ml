
let wrap parsing_f lexbuf =
  try
    Lexer.init();
    let ast = parsing_f Lexer.token lexbuf in
    ast
  with
  | Parser.Error ->
    let loc = Location.curr lexbuf in
    raise (Syntaxerr.Error(Syntaxerr.Other loc))

let expression = wrap Parser.expression
