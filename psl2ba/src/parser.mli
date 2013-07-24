type token =
  | TokenLPar
  | TokenRPar
  | TokenLBr
  | TokenRBr
  | TokenEof
  | TokenProp of (string)
  | TokenNEQ
  | TokenEQ
  | TokenEEQ
  | TokenGen
  | TokenFin
  | TokenOpO
  | TokenOpH
  | TokenNeg
  | TokenNext
  | TokenYest
  | TokenZest
  | TokenUnt
  | TokenOpW
  | TokenRel
  | TokenOpS
  | TokenOpT
  | TokenAnd
  | TokenOr
  | TokenImp
  | TokenEq
  | TokenFby
  | TokenBFby
  | TokenTrig
  | TokenBTrig
  | TokenCup
  | TokenCap
  | TokenScol
  | TokenCol
  | TokenStar
  | TokenPlus
  | TokenCl

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Psl.psl
