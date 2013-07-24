{
open Parser        (* The type token is defined in parser.mli *)
}


(* we mainly stick to the nuSMV syntax *)

rule token = parse
  [' ' '\t' '\n']    { token lexbuf }     (* skip blanks *)
  | "--"             { skip lexbuf }      (* SMV comment: skip rest of the line *)
	| '('              { TokenLPar }
  | ')'              { TokenRPar }
  | eof              { TokenEof }

  | "tt"             { TokenProp("TRUE") }   (* true and false in NuSMV *)
  | "ff"             { TokenProp("FALSE") }

  | "!="             { TokenNEQ }
	| "="              { TokenEQ }
	| "=="             { TokenEEQ }
	
	| '!'              { TokenNeg }
  | "G"              { TokenGen }
  | "F"              { TokenFin }
  | "O"              { TokenOpO }
  | "H"              { TokenOpH }
  | 'X'              { TokenNext }
  | 'Y'              { TokenYest }
  | 'Z'              { TokenZest }

  | "<>->"           { TokenFby }
  | "[]->"           { TokenTrig }
  | "<->->"          { TokenBFby }
  | "[-]->"          { TokenBTrig }

  | "{"              { TokenLBr }
  | "}"              { TokenRBr }
  | ":"              { TokenCol }
  | ";"              { TokenScol }
  | "*"              { TokenStar }
  | "+"              { TokenPlus }
  | "&&"             { TokenCap }
  | "||"             { TokenCup }
  | "Cl"             { TokenCl }

  | 'U'              { TokenUnt }
  | 'V'              { TokenRel }
	| 'W'              { TokenOpW }
	| 'R'              { TokenRel }
  | 'S'              { TokenOpS }
  | 'T'              { TokenOpT }
  | "&"              { TokenAnd }
  | "|"              { TokenOr }
  | "->"             { TokenImp }
  | "<->"            { TokenEq }

  | ['a'-'z' 'A'-'Z' '0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.' '[' ']']*
	                   { TokenProp(Lexing.lexeme lexbuf) }
  | _                { token lexbuf }     (* skip rest *)


and skip = parse
  | _                { skip lexbuf }     (* skip rest *)
  | eof              { TokenEof }
 