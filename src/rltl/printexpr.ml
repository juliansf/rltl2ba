
open Exptypes
open Exptree

module Expgen = Expgen_private

let fmt_overlap_flag f = function
  | WithOverlap -> Format.fprintf f "WithOverlap"
  | WithoutOverlap -> Format.fprintf f "WithoutOverlap"

let fmt_seq_flag f = function
  | Existential -> Format.fprintf f "Existential"
  | Universal -> Format.fprintf f "Universal"

let fmt_power_flag f = function
  | RegularPower -> Format.fprintf f "Regular"
  | WeakPower -> Format.fprintf f "Weak"
  | DualPower -> Format.fprintf f "Dual"
  | DualWeakPower -> Format.fprintf f "DualWeak"

let node_id = Expgen.node_id

let line i f s =
  Format.fprintf f "%s" (String.make (2*i) ' ');
  Format.fprintf f s

let rec print_bool mgr i ppf node =
  let exp =
    try Misc.valOf ((Manager.lookup mgr node).exp_bool)
    with
    | Not_found -> raise (Expgen.Undefined_node node)
    | Misc.ValOf_None -> raise (Expgen.Node_type_clash node)
  in
  line i ppf "bool of node %d\n" node;
  let i = i+1 in
  match exp with
  | BoolTrue -> line i ppf "BoolTrue\n"
  | BoolFalse -> line i ppf "BoolFalse\n"
  | BoolIdent (id) -> line i ppf "BoolIdent %s\n" id
  | BoolNot (n) -> line i ppf "Not\n";
    print_bool mgr i ppf n;
  | BoolOr (n1,n2) -> line i ppf "BoolOr\n";
    print_bool mgr i ppf n1;
    print_bool mgr i ppf n2;
  | BoolAnd (n1,n2) -> line i ppf "BoolAnd\n";
    print_bool mgr i ppf n1;
    print_bool mgr i ppf n2

let rec print_regex mgr i ppf node =
  let exp =
    try Misc.valOf ((Manager.lookup mgr node).exp_regex)
    with
    | Not_found -> raise (Expgen.Undefined_node node)
    | Misc.ValOf_None -> raise (Expgen.Node_type_clash node)
  in
  line i ppf "regex of node %d\n" node;
  let i = i+1 in
  match exp with
  | RegexTrue -> line i ppf "RegexTrue\n"
  | RegexFalse -> line i ppf "RegexFalse\n"
  | RegexProp (n) -> line i ppf "RegexProp\n";
    print_bool mgr i ppf n
  | RegexStar (n) -> line i ppf "RegexStar\n";
    print_regex mgr i ppf n
  | RegexPlus (n1,n2) -> line i ppf "RegexPlus\n";
    print_regex mgr i ppf n1;
    print_regex mgr i ppf n2
  | RegexCap (n1,n2) -> line i ppf "RegexCap\n";
    print_regex mgr i ppf n1;
    print_regex mgr i ppf n2
  | RegexConcat (ofl,n1,n2) ->
    line i ppf "RegexConcat %a\n" fmt_overlap_flag ofl;
    print_regex mgr i ppf n1;
    print_regex mgr i ppf n2

let rec print_rltl mgr i ppf node =
  let exp =
    try Misc.valOf ((Manager.lookup mgr node).exp_rltl)
    with
    | Not_found -> raise (Expgen.Undefined_node node)
    | Misc.ValOf_None -> raise (Expgen.Node_type_clash node)
  in
  line i ppf "rltl of node %d\n" node;
  let i = i+1 in
  match exp with
  | RltlTrue -> line i ppf "RltlTrue\n"
  | RltlFalse -> line i ppf "RltlFalse\n"
  | RltlNot (n) -> line i ppf "RltlNot\n";
    print_rltl mgr i ppf n
  | RltlProp (n) -> line i ppf "RltlProp\n";
    print_bool mgr i ppf n
  | RltlOr (n1,n2) -> line i ppf "RltlOr\n";
    print_rltl mgr i ppf n1;
    print_rltl mgr i ppf n2
  | RltlAnd (n1,n2) -> line i ppf "RltlAnd\n";
    print_rltl mgr i ppf n1;
    print_rltl mgr i ppf n2
  | RltlSeq (sfl,ofl,n1,n2) ->
    line i ppf "RltlSeq %a %a\n" fmt_seq_flag sfl fmt_overlap_flag ofl;
    print_regex mgr i ppf n1;
    print_rltl mgr i ppf n2
  | RltlPower (pfl,ofl,nx,ny,nr) ->
    line i ppf "RltlPower %a %a\n" fmt_power_flag pfl fmt_overlap_flag ofl;
    print_rltl mgr i ppf nx;
    print_rltl mgr i ppf ny;
    print_regex mgr i ppf nr
  | RltlClosure (n) -> line i ppf "RltlClosure\n";
    print_regex mgr i ppf n

let print_node mgr ppf node =
  let exp =
    try Manager.lookup mgr node
    with Not_found -> raise (Expgen.Undefined_node node)
  in
  match exp.exp_rltl with
  | Some _ -> print_rltl mgr 0 ppf node
  | None ->
    match exp.exp_regex with
    | Some _ -> print_regex mgr 0 ppf node
    | None ->
      match exp.exp_bool with
      | Some _ -> print_bool mgr 0 ppf node
      | None -> Misc.fatal_error "Printexpr.print_node"


let print_bool_expr ppf = function
  | BoolTrue -> Format.fprintf ppf "BoolTrue"
  | BoolFalse -> Format.fprintf ppf "BoolFalse"
  | BoolIdent (id) -> Format.fprintf ppf "BoolIdent(%s)" id
  | BoolNot (n) -> Format.fprintf ppf "Not(%d)" (node_id n)
  | BoolOr (n1,n2) ->
    Format.fprintf ppf "BoolOr(%d,%d)" (node_id n1) (node_id n2)
  | BoolAnd (n1,n2) ->
    Format.fprintf ppf "BoolAnd %d %d" (node_id n1) (node_id n2)

let print_regex_expr ppf = function
  | RegexTrue -> Format.fprintf ppf "RegexTrue"
  | RegexFalse -> Format.fprintf ppf "RegexFalse"
  | RegexProp (n) -> Format.fprintf ppf "RegexProp(%d)" (node_id n)
  | RegexStar (n) -> Format.fprintf ppf "RegexStar(%d)" (node_id n)
  | RegexPlus (n1,n2) ->
    Format.fprintf ppf "RegexPlus(%d,%d)" (node_id n1) (node_id n2)
  | RegexCap (n1,n2) ->
    Format.fprintf ppf "RegexCap(%d,%d)" (node_id n1) (node_id n2)
  | RegexConcat (ofl,n1,n2) ->
    Format.fprintf ppf "RegexConcat(%a,%d,%d)"
      fmt_overlap_flag ofl (node_id n1) (node_id n2)

let print_rltl_expr ppf = function
  | RltlTrue -> Format.fprintf ppf "RltlTrue"
  | RltlFalse -> Format.fprintf ppf "RltlFalse"
  | RltlNot (n) -> Format.fprintf ppf "RltlNot(%d)" (node_id n)
  | RltlProp (n) -> Format.fprintf ppf "RltlProp(%d)" (node_id n)
  | RltlOr (n1,n2) ->
    Format.fprintf ppf "RltlOr(%d,%d)" (node_id n1) (node_id n2)
  | RltlAnd (n1,n2) ->
    Format.fprintf ppf "RltlAnd(%d,%d)" (node_id n1) (node_id n2)
  | RltlSeq (sfl,ofl,n1,n2) ->
    Format.fprintf ppf "RltlSeq(%a,%a,%d,%d)"
      fmt_seq_flag sfl fmt_overlap_flag ofl (node_id n1) (node_id n2)
  | RltlPower (pfl,ofl,nx,ny,nr) ->
    Format.fprintf ppf "RltlPower(%a,%a,x:%d,y:%d,r:%d)"
      fmt_power_flag pfl fmt_overlap_flag ofl
      (node_id nx) (node_id ny) (node_id nr)
  | RltlClosure (n) -> Format.fprintf ppf "RltlClosure(%d)" (node_id n)

let print_expr i ppf {exp_bool;exp_regex;exp_rltl} =
  let pp_option f ppf = function
  | None -> Format.fprintf ppf "None"
  | Some e -> Format.fprintf ppf "Some (%a)" f e
  in
  line i ppf "{\n";
  line (i+1) ppf "bool: %a;\n" (pp_option print_bool_expr) exp_bool;
  line (i+1) ppf "regex: %a;\n" (pp_option print_regex_expr) exp_regex;
  line (i+1) ppf "rltl: %a;\n" (pp_option print_rltl_expr) exp_rltl;
  line i ppf "}\n"

let print_manager ppf mgr =
  let f n e =
    line 0 ppf "%d:\n" (node_id n);
    print_expr 1 ppf e
  in
  let size = Manager.size mgr in
  (*let mgr_array = Array.init size (fun i -> Manager.lookup mgr i) in*)
  for i=0 to size-1 do
    (*f i mgr_array.(i)*)
    f i (Manager.lookup mgr i)
  done
