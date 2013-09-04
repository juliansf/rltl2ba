open Parsetree
open OUnit

let compare cmp mk e1 e2 =
  match mk e1, mk e2 with
  | Some ast1, Some ast2 -> cmp ast1 ast2
  | _ -> false

let expression_of_string str =
  try
    let lexbuf = Lexing.from_string str in
    Some (Parse.expression lexbuf)
  with e ->
    Errors.report_error Format.err_formatter e;
    None

let rec cmp_expression exp1 exp2 =
  let open Asttypes in
  let rec cmp_ty t1 t2 =
    match t1.pty_desc,t2.pty_desc with
    | Pty_name i1, Pty_name i2 -> i1.txt = i2.txt
    | Pty_arrow (t1,t1'), Pty_arrow (t2,t2') ->
      cmp_ty t1 t2 && cmp_ty t1' t2'
    | _ -> false
  in

  let cmp_var v1 v2 =
    match v1.pvar_desc, v2.pvar_desc with
    | Pvar_ident i1, Pvar_ident i2 -> i1.txt = i2.txt
    | Pvar_funct (f1,args1), Pvar_funct (f2, args2) ->
      f1.txt = f2.txt
      && List.for_all2 (fun (x1,t1) (x2,t2) ->
        x1.txt = x2.txt
        && cmp_ty t1 t2) args1 args2
    | _ -> false
  in
  match exp1.pexp_desc, exp2.pexp_desc with
  | Pexp_boolconst s1, Pexp_boolconst s2 -> s1 = s2
  | Pexp_ident s1, Pexp_ident s2 -> s1 = s2
  | Pexp_apply (e1,l1), Pexp_apply (e2,l2) ->
    if (cmp_expression e1 e2) then
      try
        List.for_all2 cmp_expression l1 l2
      with _ -> false
    else false
  | Pexp_power (f1, x1, y1, r1), Pexp_power (f2, x2, y2, r2) ->
    (f1 = f2) &&
      (match r1, r2 with
      | None, None -> true
      | Some e1, Some e2 -> cmp_expression e1 e2
      | _ -> false) &&
      cmp_expression x1 x2 && cmp_expression y1 y2
  | Pexp_overlap e1, Pexp_overlap e2 -> cmp_expression e1 e2
  | Pexp_closure e1, Pexp_overlap e2 -> cmp_expression e1 e2
  | Pexp_let ((v1,x1), e1), Pexp_let ((v2,x2), e2) ->
    (cmp_var v1 v2) && (cmp_expression x1 x2) && (cmp_expression e1 e2)
  | _ -> false

let cmpe e1 e2 =
  compare cmp_expression expression_of_string e1 e2

let e_assert_equal e1 e2 =
  assert_bool "cmpe_false" (cmpe e1 e2)

let test_equality_list = [
  (* Delimiters *)
  ( "begin {(p)} end", "p" );

  (* The precedence of the operators help to avoid some test cases *)

  (* Boolean expressions *)
  (* Precedence: & << | << <-> << -> *)
  "!a=b", "(!a)=b";
  "a=b & c!=d", "(a=b) & (c!=d)";
  "!a & !b", "(!a) & (!b)";
  "a & b | c & d", "(a & b) | (c & d)";
  "a -> b -> c", "a -> (b -> c)";
  "a | b -> c <-> d", "(a | b) -> (c <-> d)";

  (* Regular expressions *)
  (* Precedence * << ' ' << ; << && << + *)
  "a*b", "(a*)b";
  "a b*", "a (b*)";
  "a*b", "(a*)b";
  (* "a*b", "a*;b"; *)
  "a b c", "a (b c)";
  "a b ; c d", "(a b); (c d)";
  "a -> b c", "a -> (b c)";
  "a -> b; c", "(a -> b); c";
  "a -> b && c -> d", "(a -> b) && (c -> d)";
  "a b && c d", "(a b) && (c d)";
  "a && b ; c", "(a && b);c";
  "a && b + c && d", "(a && b) + (c && d)";
  "a -> b <-> c", "a -> (b <-> c)";
  "a*b + c", "(a*b) + c";
  (* "a b c", "a;b;c";*)

  (* RLTL expressions  *)
  "not a + b", "not (a + b)";
  "not a b", "not (a b)";
  "a ; b and c : d", "(a ; b) and (c : d)";
  "a and b or c and d", "(a and b) or (c and d)";
  "a or b iff c or d", "(a or b) iff (c or d)";
  "a implies b implies c", "a implies (b implies c)";
  "a or b implies c iff d", "(a or b) implies (c iff d)";
  "a implies b until c", "a implies (b until c)";
  "a until b with delay r and c", "(a until b with delay r) and (c)";
  "a until b until c", "a until (b until c)";
  "a until b until c with delay r", "a until (b until c with delay r)";
  "a until b until c with delay r with delay s", "a until (b until c with delay r) with delay s";

  (* LET *)
  "let x = a until b in x and b", "let x = (a until b) in { x and b }";

  (* Apply *)
  "let U(x:rltl, y:regex, r:bool) = a in U.(a,b).(c)",
  "let U(x:rltl, y:regex, r:bool) = a in U.(a,b,c)";
]

let map_list_to_test_list l =
  let i = ref 0 in
  let tname a = incr i; "test"^(string_of_int !i)^": "^a in
  List.map (fun (a,b) -> tname a >:: (fun () -> e_assert_equal a b)) l

let suite =
  "syntax" >::: map_list_to_test_list test_equality_list

let _ =
  run_test_tt_main suite
