open Parsetree
open Location
open Lexing
open Asttypes

let show_loc = ref true

let fmt_position f l =
  if l.pos_lnum = -1
  then Format.fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else Format.fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
    (l.pos_cnum - l.pos_bol)

let fmt_location f loc =
  if !show_loc then begin
    Format.fprintf f "(%a..%a)" fmt_position loc.loc_start
      fmt_position loc.loc_end
  end;
  if loc.loc_ghost then Format.fprintf f " ghost"

let line i f s =
  Format.fprintf f "%s" (String.make (2*i) ' ');
  Format.fprintf f s

let fmt_ident f s =
  Format.fprintf f "\"%s\"" s

let fmt_power_flag f = function
  | Until -> Format.fprintf f "Until"
  | WeakUntil -> Format.fprintf f "WeakUntil"
  | Release -> Format.fprintf f "Release"
  | StrongRelease -> Format.fprintf f "StrongRelease"

let list i f ppf = function
  | [] -> line i ppf "[]\n"
  | _::_ as l ->
    line i ppf "[\n";
    List.iter (f (i+1) ppf) l;
    line i ppf "]\n"

let option i f ppf = function
  | None -> line i ppf "None\n"
  | Some x ->
    line i ppf "Some\n";
    f (i+1) ppf x

let rec ty i ppf t =
  line i ppf "ty %a\n" fmt_location t.pty_loc;
  let i = i+1 in
  match t.pty_desc with
  | Pty_name (name) ->
    line i ppf "Pty_name %a\n" fmt_ident name.txt
  | Pty_arrow(t1,t2) ->
    line i ppf "Pty_arrow\n";
    ty i ppf t1;
    ty i ppf t2

let var i ppf v =
  line i ppf "var %a\n" fmt_location v.pvar_loc;
  let i = i+1 in
  match v.pvar_desc with
  | Pvar_ident (id) ->
    line i ppf "Pvar_ident %a\n" fmt_ident id.txt
  | Pvar_funct (f,args) ->
    line i ppf "Pvar_funct %a\n" fmt_ident f.txt;
    List.iter (fun (s,t) ->
      line (i+1) ppf "Arg %a:\n" fmt_ident s.txt;
      ty (i+2) ppf t) args

let rec expression i ppf e =
  line i ppf "expression %a\n" fmt_location e.pexp_loc;
  let i = i+1 in
  match e.pexp_desc with
  | Pexp_boolconst (c) -> line i ppf "Pexp_boolconst %a\n" fmt_ident c
  | Pexp_ident (id) -> line i ppf "Pexp_ident %a\n" fmt_ident id
  | Pexp_apply (e, l) ->
    line i ppf "Pexp_apply\n";
    expression i ppf e;
    list i expression ppf l
  | Pexp_power (pf, x, y, ro) ->
    line i ppf "Pexp_power %a\n" fmt_power_flag pf;
    expression i ppf x;
    expression i ppf y;
    option i expression ppf ro
  | Pexp_overlap e ->
    line i ppf "Pexp_overlap\n";
    expression i ppf e
  | Pexp_closure e ->
    line i ppf "Pexp_closure\n";
    expression i ppf e
  | Pexp_let (d, e) ->
    line i ppf "Pexp_let\n";
    list i ident_x_expression ppf [d];
    expression i ppf e

and ident_x_expression i ppf (v, e) =
  line i ppf "<def>\n";
  var (i+1) ppf v;
  expression (i+1) ppf e

let print_expr ppf e =
  show_loc := true; expression 0 ppf e

let print_expr_no_loc ppf e =
  show_loc := false; expression 0 ppf e
