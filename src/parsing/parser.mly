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
  (* must be called [mkinfix e1 op e2 $startpos(op) $endpos(op)*)

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
%token AMPERAMPER
%token AMPERSAND
%token AND
%token BANG
%token BANGEQUAL
%token BAR
%token BARBAR
%token BEGIN
%token CLOSURE
%token COLON
%token COLONCOLON
%token COMMA
%token DELAY
%token DOT
%token END
%token EOF
%token EQUAL
%token FALSE
%token <string> IDENT
%token IFF
%token IMPLIES
%token IN
%token LB
%token LESSMINUSGREATER
%token LET
%token LP
%token MINUSGREATER
%token NOT
%token OR
%token OVERLAP
%token PLUS
%token RB
%token RELEASE
%token RP
%token SEMI
%token SEMISEMI
%token SRELEASE
%token STAR
%token TRUE
%token UNTIL
%token WITH
%token WUNTIL

/* Precedences from low to high */
%nonassoc IN
/*%nonassoc below_SEMI*/
/*%nonassoc CLOSURE OVERLAP*/
/*%nonassoc LET*/
%right IMPLIES
%left IFF
%left OR
%left AND
%nonassoc below_WITH
%nonassoc WITH DELAY
/*%nonassoc below_UNTIL*/
%nonassoc UNTIL WUNTIL RELEASE SRELEASE
%nonassoc NOT
%right SEMI COLON SEMISEMI COLONCOLON
%left PLUS BARBAR
%left AMPERAMPER
%right below_STAR
%right STAR
%right MINUSGREATER
%left LESSMINUSGREATER
%left BAR
%left AMPERSAND
/*%nonassoc below_EQUAL*/
%left EQUAL BANGEQUAL

%nonassoc BANG BEGIN DOT FALSE IDENT LB LP TRUE

/* Entry points */

%start expression
%type <Parsetree.expression> expression

%%

/* Rules for entry points */

expression:
  e = expr EOF { e }
;

expr:
  e = simple_expr
    { e }
| LET lb=let_binding IN e=expr
    { mkexp (Pexp_let (lb, e)) $startpos $endpos }
| l=expr op=oper r=expr
    { mkinfix l op r $startpos(op) $endpos(op) }
| NOT e=expr
    {
      mkexp (Pexp_apply(mkoper "not" $startpos($1) $endpos($1), [e]))
        $startpos $endpos
    }
| x=expr pf=power_oper y=expr %prec below_WITH
    { mkexp (Pexp_power (pf, x, y, None)) $startpos $endpos }
| x=expr pf=power_oper y=expr WITH DELAY r=expr
    { mkexp (Pexp_power (pf, x, y, Some r)) $startpos $endpos }
| expr power_oper expr WITH error
    { expected "delay" $startpos($5) $endpos($5) }
| oper=prefix_oper LP e=expr RP
    { mkexp (oper e) $startpos $endpos }
| prefix_oper LP expr error
    { unclosed "(" $startpos($2) $endpos($2) ")" $startpos($4) $endpos($4) }
| prefix_oper error
    { expected "(" $startpos($2) $endpos($2) }

| f=expr DOT LP args=expr_comma_list RP
    { mkexp (Pexp_apply (f, args)) $startpos $endpos }
;

simple_expr:
  TRUE
    { mkexp (Pexp_boolconst "true") $startpos $endpos }
| FALSE
    { mkexp (Pexp_boolconst "false") $startpos $endpos }
| id=ident
    { mkexp (Pexp_ident id) $startpos $endpos }
| LB e=expr RB
| LP e=expr RP
| BEGIN e=expr END
    { reloc_exp e $startpos(e) $endpos(e) }
| LP expr error
    { unclosed "(" $startpos($1) $endpos($1) ")" $startpos($3) $endpos($3) }
| LB expr error
    { unclosed "{" $startpos($1) $endpos($1) "}" $startpos($3) $endpos($3) }
| BEGIN expr error
    { unclosed "begin" $startpos($1) $endpos($1) "end" $startpos($3) $endpos($3) }
| BANG e=simple_expr
    {
      mkexp (Pexp_apply (mkoper "!" $startpos($1) $endpos($1), [e]))
        $startpos $endpos
    }
| e=simple_expr STAR
    {
      mkexp (Pexp_apply (mkoper "*" $startpos($2) $endpos($2), [e]))
        $startpos $endpos
    }
| l=simple_expr r=simple_expr %prec below_STAR
    { ghexp (Pexp_apply (mkoper "." $startpos $endpos, [l;r])) $startpos $endpos }
;

/*
simple_expr_list:
  e=simple_expr
    { [e] }
| e=simple_expr e=simple_expr_list
*/

expr_comma_list:
  e=expr
    { [e] }
| e=expr COMMA el=expr_comma_list
    { e :: el }

let_binding:
  id=ident EQUAL e=expr
    { (mkidentvar id $startpos(id) $endpos(id), e) }
| f=ident LP l=ident_type_list RP EQUAL e=expr
    { (mkfunctvar f l $startpos(f) $endpos(f) $startpos $endpos, e) }
| ident LP ident_type_list error
    { unclosed "(" $startpos($2) $endpos($2) ")" $startpos($4) $endpos($4) }

;

ident:
  id=IDENT
    { id }
;

ty:
  id=ident
    { mktyname id $startpos(id) $endpos(id) }
| t1=ty MINUSGREATER t2=ty
    { mktyarrow t1 t2 $startpos $endpos }
|  LP t=ty RP
    { t }
| LP ty error
    { unclosed "(" $startpos($1) $endpos($1) ")" $startpos($3) $endpos($3) }
;

ident_type:
  id=ident COLON ty=ty
    { (mkrhs id $startpos(id) $endpos(id), ty) }
;

ident_type_list:
  vt=ident_type
    { [vt] }
| vt=ident_type COMMA vts=ident_type_list
    { vt::vts }
;

%inline oper:
  AMPERSAND { "&" }
| AMPERAMPER { "&&" }
| AND { "and" }
| BANGEQUAL { "!=" }
| BAR { "|" }
| BARBAR { "||" }
| COLON { ":" }
| COLONCOLON { "::" }
| EQUAL { "=" }
| IFF { "iff" }
| IMPLIES { "implies" }
| LESSMINUSGREATER { "<->" }
| MINUSGREATER { "->" }
| OR { "or" }
| PLUS { "+" }
| SEMI { ";" }
| SEMISEMI { ";;" }
;

%inline power_oper:
  UNTIL { Until }
| WUNTIL { WeakUntil }
| RELEASE { Release }
| SRELEASE {StrongRelease }
;

%inline prefix_oper:
  OVERLAP { fun e -> Pexp_overlap e }
| CLOSURE { fun e -> Pexp_closure e }
