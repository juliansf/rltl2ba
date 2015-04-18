%{
  open Location
  open Asttypes
  open Parsetree

  let mkexp d s e =
    { pexp_desc = d; pexp_loc = symbol_rloc s e}

  let mkrhs rhs s e = mkloc rhs (symbol_rloc s e)
  (* must be called [mkrhs id $startpos(id) $endpos(id)] *)

  let ghexp d s e =
    { pexp_desc = d; pexp_loc = symbol_gloc s e }

  let mktyname t s e =
    { pty_desc = Pty_name (mkrhs t s e); pty_loc = symbol_rloc s e }

  let mktyarrow t1 t2 s e =
    { pty_desc = Pty_arrow (t1,t2); pty_loc = symbol_rloc s e }

  let mkidentvar v s e =
    { pvar_desc = Pvar_ident (mkrhs v s e); pvar_loc = symbol_rloc s e }

  let mkfunctvar f l sf ef s e =
    { pvar_desc = Pvar_funct (mkrhs f sf ef, l); pvar_loc = symbol_rloc s e }

  let mkoper name s e =
    { pexp_desc = Pexp_ident name; pexp_loc = symbol_rloc s e }

  let mkinfix arg1 name arg2 s e =
    let istart = arg1.pexp_loc.loc_start in
    let iend = arg2.pexp_loc.loc_end in
    mkexp (Pexp_apply (mkoper name s e, [arg1; arg2])) istart iend
  (* must be called [mkinfix e1 op e2 $startpos(op) $endpos(op)]*)

  let reloc_exp x s e = { x with pexp_loc = symbol_rloc s e }

  let unclosed oname ostart oend cname cstart cend =
    let opening_loc = symbol_rloc ostart oend in
    let closing_loc = symbol_rloc cstart cend in
    raise (Syntaxerr.Error(Syntaxerr.Unclosed(opening_loc, oname,
                                              closing_loc, cname)))
  let expected name s e =
    raise (Syntaxerr.Error(Syntaxerr.Expected(symbol_rloc s e, name)))
%}

/* Tokens */
%token ALWAYS
%token AMPERAMPER
%token AMPERSAND
%token BANG
%token BAR
%token BARBAR
%token BARIMPL
%token COLON
%token DIAMONDIMPL
%token EOF
%token EVENTUALLY
%token FALSE
%token HAT
%token <string> IDENT
%token LB
%token LESSMINUSGREATER
%token LP
%token MINUSGREATER
%token MODULE
%token NEVER
%token NEXT
%token PLUS
%token PSLSPEC
%token RELEASE
%token RB
%token RP
%token SEMI
%token STAR
%token TRUE
%token UNTIL
%token VAR
%token XOR

/* Precedences from low to high */
%left UNTIL RELEASE
%right BARIMPL DIAMONDIMPL
%right MINUSGREATER
%left LESSMINUSGREATER
%left BARBAR PLUS
%left AMPERAMPER XOR
%left BAR HAT
%left AMPERSAND
%right SEMI COLON
%right STAR

%nonassoc EVENTUALLY ALWAYS BANG NEVER NEXT


/* Entry points */

%start expression
%type <Parsetree.expression> expression

%%

/* Rules for entry points */
expression:
| e = psl_expr option(SEMI) EOF { e }
| MODULE ident vars_decl PSLSPEC e=psl_expr option(SEMI) EOF { e }
;

var_decl:
  VAR ident COLON ident SEMI
    {()}
;

vars_decl:
  list(var_decl)
    {()}
;

psl_expr:
  TRUE
    { mkexp (Pexp_boolconst "true") $startpos $endpos }
| FALSE
    { mkexp (Pexp_boolconst "false") $startpos $endpos }
| id=ident
    { mkexp (Pexp_ident id) $startpos $endpos }

| LB r = regex_expr RB
    { r }

| LP e=psl_expr RP
    { e }

| BANG e=psl_expr
    {
      mkexp (Pexp_apply (mkoper "not" $startpos($1) $endpos($1), [e]))
        $startpos $endpos
    }

| x=psl_expr op=psl_oper y=psl_expr
    {
      mkinfix x op y $startpos(op) $endpos(op)
    }

| NEXT e=psl_expr
    {
      mkexp (Pexp_apply (mkoper ";" $startpos($1) $endpos($1),
        [mkexp (Pexp_boolconst "true") $startpos $endpos; e]))
        $startpos $endpos
    }
| x=psl_expr UNTIL y=psl_expr
    {
      mkexp (Pexp_power (Until, x, y, None)) $startpos $endpos
    }
| x=psl_expr RELEASE y=psl_expr
    {
      mkexp (Pexp_power (Release, x, y, None)) $startpos $endpos
    }
| EVENTUALLY x=psl_expr
    { mkexp (Pexp_power (Until,
        mkexp (Pexp_boolconst "true") $startpos $endpos,
        x, None))
        $startpos $endpos }
| ALWAYS x=psl_expr
    { mkexp (Pexp_power (WeakUntil, x,
        mkexp (Pexp_boolconst "false") $startpos $endpos, None))
        $startpos $endpos }
| NEVER x=psl_expr
    {
      mkexp (Pexp_power (WeakUntil,
        (mkexp (Pexp_apply (mkoper "not" $startpos($1) $endpos($1), [x]))
        $startpos $endpos),
        mkexp (Pexp_boolconst "false") $startpos $endpos, None))
        $startpos $endpos
    }
| LB r=regex_expr RB op=BARIMPL x=psl_expr
    {
      mkexp (Pexp_apply (mkoper "::" $startpos(op) $endpos(op), [r;x]))
        $startpos $endpos
    }
| LB r=regex_expr RB op=DIAMONDIMPL x=psl_expr
    {
      mkexp (Pexp_apply (mkoper ":" $startpos(op) $endpos(op), [r;x]))
        $startpos $endpos
    }
| LB regex_expr error
    { unclosed "{" $startpos($1) $endpos($1) "}" $startpos($3) $endpos($3) }
;

%inline psl_oper:
  AMPERSAND { "and" }
| BAR { "or" }
| MINUSGREATER { "implies" }
| LESSMINUSGREATER { "iff" }
| XOR { "xor" }
;

regex_expr:
  TRUE
    { mkexp (Pexp_boolconst "true") $startpos $endpos }
| FALSE
    { mkexp (Pexp_boolconst "false") $startpos $endpos }
| id=ident
    { mkexp (Pexp_ident id) $startpos $endpos }
| x=regex_expr op=oper y=regex_expr
    {
      mkinfix x op y $startpos(op) $endpos(op)
    }
| LB e=regex_expr RB
| LP e=regex_expr RP
    { reloc_exp e $startpos(e) $endpos(e) }
| LP regex_expr error
    { unclosed "(" $startpos($1) $endpos($1) ")" $startpos($3) $endpos($3) }
| LB regex_expr error
    { unclosed "{" $startpos($1) $endpos($1) "}" $startpos($3) $endpos($3) }
| e=regex_expr STAR
    {
      mkexp (Pexp_apply (mkoper "*" $startpos($2) $endpos($2), [e]))
        $startpos $endpos
    }
| BANG e=regex_expr
    {
      mkexp (Pexp_apply (mkoper "!" $startpos($1) $endpos($1), [e]))
        $startpos $endpos
    }
;

ident:
  id=IDENT
    { id }
;


%inline oper:
  AMPERSAND { "&" }
| AMPERAMPER { "&&" }
| BAR { "|" }
| BARBAR { "||" }
| COLON { ":" }
| HAT { "^" }
| LESSMINUSGREATER { "<->" }
| MINUSGREATER { "->" }
| PLUS { "+" }
| SEMI { ";" }
| XOR  { "^" }
;
