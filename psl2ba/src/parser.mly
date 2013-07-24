/* The Header */
%{
open Psl
%}

/* The Grammar */
%token TokenLPar
%token TokenRPar
%token TokenLBr
%token TokenRBr
%token TokenEof

%token <string> TokenProp
%token TokenNEQ
%token TokenEQ
%token TokenEEQ

%token TokenGen
%token TokenFin
%token TokenOpO
%token TokenOpH
%token TokenNeg
%token TokenNext
%token TokenYest
%token TokenZest

%token TokenUnt
%token TokenOpW
%token TokenRel
%token TokenOpS
%token TokenOpT
%token TokenAnd
%token TokenOr
%token TokenImp
%token TokenEq

%token TokenFby
%token TokenBFby
%token TokenTrig
%token TokenBTrig

%token TokenCup
%token TokenCap
%token TokenScol
%token TokenCol
%token TokenStar
%token TokenPlus
%token TokenCl


/* lowest precedence */
%right TokenImp 
%left TokenEq
%left TokenCup TokenOr
%left TokenCap TokenAnd
%left TokenScol TokenCol
%right TokenFby TokenTrig TokenBFby TokenBTrig
%left TokenUnt TokenRel TokenOpS TokenOpT
%nonassoc TokenFin TokenOpO TokenGen TokenOpH TokenNext TokenYest TokenZest
%right TokenStar
%nonassoc TokenNeg
%nonassoc TokenCl
%left TokenEQ TokenNEQ
/* highest precedence */

%start main             /* the entry point */
%type <Psl.psl> main

%%

main:
   expr TokenEof             { $1 }
;

expr:
  | TokenLPar expr TokenRPar   { $2 }

	| TokenProp                { Prop $1 }
	| TokenProp TokenEQ TokenProp  { Prop ("("^$1^"="^$3^")") }
	| TokenProp TokenEEQ TokenProp  { Prop ("("^$1^"=="^$3^")") }
	| TokenProp TokenNEQ TokenProp  { Prop ("("^$1^"!="^$3^")") }

  | TokenFin expr            { OpF $2 }
	| TokenGen expr            { OpG $2 }
  | TokenOpO expr            { OpO $2 }
  | TokenOpH expr            { OpH $2 }
  | TokenNeg expr            { OpNeg $2 }
  | TokenNext expr           { OpX $2 }
  | TokenYest expr           { OpY $2 }
  | TokenZest expr           { OpZ $2 }

  | expr TokenUnt expr       { OpU ($1, $3) }
  | expr TokenOpW expr       { OpOr (OpU ($1, $3), OpG $1) }
  | expr TokenRel expr       { OpR ($1, $3) }
  | expr TokenOpS expr       { OpS ($1, $3) }
  | expr TokenOpT expr       { OpT ($1, $3) }
  | expr TokenAnd expr       { OpAnd ($1, $3) }
  | expr TokenOr expr        { OpOr ($1, $3) }
  | expr TokenImp expr       { OpOr (OpNeg $1, $3) }
  | expr TokenEq expr        { OpAnd (OpOr (OpNeg $1, $3), OpOr ($1, OpNeg $3)) }

	| TokenLBr sere TokenRBr TokenFby expr       { Fby ($2, $5) }
  | TokenLBr sere TokenRBr TokenBFby expr      { BFby ($2, $5) }
  | TokenLBr sere TokenRBr TokenTrig expr      { Trig ($2, $5) }
  | TokenLBr sere TokenRBr TokenBTrig expr     { BTrig ($2, $5) }
  | TokenCl TokenLBr sere TokenRBr             { Cl $3 }
;

sere:
  TokenLBr sere TokenRBr     { $2 }
  | bexp                     { BExp $1 }
	| sere TokenStar           { Star $1 }
	| sere TokenPlus           { Conc($1, Star $1) }
  | sere TokenCol sere       { Col ($1, $3) } 
  | sere TokenScol sere      { Conc ($1, $3) }
  | sere TokenCap sere       { Cap ($1, $3) }
  | sere TokenCup sere       { Cup ($1, $3) }
;

bexp:
  TokenLPar bexp TokenRPar   { $2 }
	| TokenProp                { Atom $1 }
	| TokenProp TokenEQ TokenProp  { Atom ($1^"="^$3) }
	| TokenProp TokenNEQ TokenProp  { Atom ($1^"!="^$3) }
  | TokenNeg bexp            { Neg $2 }
  | bexp TokenAnd bexp       { And ($1, $3) }
  | bexp TokenOr bexp        { Or ($1, $3) }
;
