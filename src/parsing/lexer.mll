{
  open Lexing
  open Parser
  open Misc

  type error =
  | Illegal_character of char
  | Unterminated_comment of Location.t
  | Literal_overflow of string
  | Keyword_as_label of string

  exception Error of error * Location.t

(* Error report *)
  let report_error ppf = function
    | Illegal_character c ->
      Format.fprintf ppf "Illegal character (%s)" (Char.escaped c)
    | Unterminated_comment _ ->
      Format.fprintf ppf "Comment not terminated"
    | Literal_overflow ty ->
      Format.fprintf ppf "Integer literal exceeds the range of representable integers"
    | Keyword_as_label kwd ->
      Format.fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd

(* Keywords table *)
  let kwd_table =
    create_hashtable 22 [
      "and", AND;
      "begin", BEGIN;
      "Cl", CLOSURE;
      "delay", DELAY;
      "end", END;
      "false", FALSE;
      "ff", FALSE;
      "iff", IFF;
      "implies", IMPLIES;
      "in", IN;
      "let", LET;
      "not", NOT;
      "or", OR;
      "overlap", OVERLAP;
      "overlapped_release", ORELEASE;
      "overlapped_strong_release", OSRELEASE;
      "overlapped_until", OUNTIL;
      "overlapped_weak_until", OWUNTIL;
      "release", RELEASE;
      "R", RELEASE;
      "strong_release", SRELEASE;
      "true", TRUE;
      "tt", TRUE;
      "until", UNTIL;
      "U", UNTIL;
      "wait_for", WUNTIL;
      "W", WUNTIL;
      "weak_until", WUNTIL;
      "with", WITH;
      "xor", XOR;
    ]

(* Update the current location with file name and line number *)
(* - from Ocaml.Lexer module *)

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
      | None -> pos.pos_fname
      | Some s -> s
    in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

  let init () = ()
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal = (['0'-'9'] ['0'-'9' '_'] *)
let int_literal = decimal_literal

rule token = parse
| newline
    {
      update_loc lexbuf None 1 false 0;
      token lexbuf
    }
| blank +
    {
      token lexbuf
    }

(* Comments *)
| "--"
    {
      comment lexbuf; token lexbuf
    }

(* Keywords and identifiers *)
| (lowercase | uppercase) identchar * as s
    {
      try
        Hashtbl.find kwd_table s
      with Not_found -> IDENT s
    }
(*
| uppercase identchar * as s
    {
      IDENT s
    }
*)
(* Symbols *)
| "."   { DOT }
| ","   { COMMA }
| "!="  { BANGEQUAL }
| "="   { EQUAL }
| "!"   { BANG }
| "->"  { MINUSGREATER }
| "<->" { LESSMINUSGREATER }

| ";;"  { SEMISEMI }
| "::"  { COLONCOLON }
| ";"   { SEMI }
| ":"   { COLON }
| "&"   { AMPERSAND }
| "|"   { BAR }
| "^"   { HAT }
| "&&"  { AMPERAMPER }
| "||"  { BARBAR }
| "*"   { STAR }
| "+"   { PLUS }

(* Delimiters *)
| '('   { LP }
| ')'   { RP }
| '{'   { LB }
| '}'   { RB }

| eof
    { EOF }
| _
    {
      raise (Error (Illegal_character (Lexing.lexeme_char lexbuf 0),
                    Location.curr lexbuf))
    }

(* Process the entire line of the comment *)
and comment = parse
| newline
    {
      update_loc lexbuf None 1 false 0
    }
| eof { () }
| _
    {
      comment lexbuf
    }
