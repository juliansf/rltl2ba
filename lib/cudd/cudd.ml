
type manager
type node

type tbool = False | True | Top

external create: unit -> manager =
  "caml_cudd_manager_make"

external size: manager -> int =
  "caml_cudd_manager_size"

external dtrue: manager -> node =
  "caml_cudd_bdd_dtrue"

external dfalse: manager -> node =
  "caml_cudd_bdd_dfalse"

external ithvar: manager -> int -> node =
  "caml_cudd_bdd_ithvar"

external newvar: manager -> node =
  "caml_cudd_bdd_newvar"

external newvar_at_level: manager -> int -> node =
  "caml_cudd_bdd_newvar_at_level"

external index: node -> int =
  "caml_cudd_bdd_index"

(*
external count: node -> int =
  "caml_cudd_bdd_count"
*)

external is_true: node -> bool =
  "caml_cudd_bdd_is_true"

external is_false: node -> bool =
  "caml_cudd_bdd_is_false"

external dnot: node -> node =
  "caml_cudd_bdd_dnot"

external dand: node -> node -> node =
  "caml_cudd_bdd_dand"

external dor: node -> node -> node =
  "caml_cudd_bdd_dor"

external compose: node -> node -> int -> node =
  "caml_cudd_bdd_compose"

external restrict: node -> node -> node =
  "caml_cudd_bdd_restrict"

external constrain: node -> node -> node =
  "caml_cudd_bdd_constrain"

external find_essential: node -> node =
  "caml_cudd_bdd_find_essential"

external iter_cube: (tbool array -> unit) -> node -> unit =
  "caml_cudd_bdd_iter_cube"

(*external first_cube: node -> tbool array =
  "caml_cudd_bdd_first_cube"*)

external bdd_cubes: node -> tbool array list =
  "caml_cudd_bdd_cubes"

(*external bdd_restricted_cubes: node -> tbool array list =
  "caml_cudd_bdd_restricted_cubes"*)

let print_minterm print_id fmt bdd =
  let _print fmt bdd =
    if is_true bdd then Format.fprintf fmt "true"
    else if is_false bdd then Format.fprintf fmt "false"
    else begin
    Format.fprintf fmt "@[<hov>";
    let first = ref true in
    iter_cube
      (begin fun cube ->
	if not !first then
	  Format.fprintf fmt " |@ @[<hov>"
	else begin
	  first := false;
	  Format.fprintf fmt "@[<hov>"
	end;
	let firstm = ref true in
	Array.iteri
	  (begin fun i elt ->
	    match elt with
	    | False ->
	      if not !firstm then
                Format.fprintf fmt " & @," else firstm := false;
	      Format.fprintf fmt "!%a" print_id i
	    | True ->
	      if not !firstm then
                Format.fprintf fmt " & @," else firstm := false;
	      Format.fprintf fmt "%a" print_id i
	    | Top -> ()
	   end)
	  cube;
	Format.fprintf fmt "@]"
       end)
      bdd;
    Format.fprintf fmt "@]"
    end
  in
  _print fmt bdd

let fold_minterm (_true: 'a) (_false: 'a) (_var: int -> 'a) (_not: 'a -> 'a)
    (_and: 'a -> 'a -> 'a) (_or: 'a -> 'a -> 'a) (bdd: node) =
  if is_true bdd then _true
  else if is_false bdd then _false
  else begin
    let cubes = bdd_cubes bdd in

    (* Set the vars *)
    let mapvar i = function
      | False -> _not (_var i)
      | True -> _var i
      | Top -> _true
    in
    let f_or a cube =
      let cube = Array.mapi mapvar cube in
      let b = Array.fold_left _and _true cube in
      _or a b
    in
    List.fold_left f_or _false (List.rev cubes)
  end

let bdd_print_wff varmap fmt bdd =
  let bdd2boolean bdd =
    let _var i = `V (varmap i) in
    let _not b = `N b in
    let _and a b = match a,b with
      | `F,_ | _,`F -> `F
      | `T,x | x,`T -> x
      | x,y -> `A (x,y)
    in
    let _or a b = match a,b with
      | `T,_ | _,`T -> `T
      | `F,x | x,`F -> x
      | x,y -> `O (x,y)
    in
    fold_minterm `T `F _var _not _and _or bdd in

  let print_boolean fmt b =
    let rec print_b fmt = function
      | `T -> Format.fprintf fmt "true"
      | `F -> Format.fprintf fmt "false"
      | `V s -> Format.fprintf fmt "%s" s
      | `N x -> Format.fprintf fmt "!%a" print_b x
      | `A (a,b) -> Format.fprintf fmt "%a & %a" print_b a print_b b
      | `O (a,b) -> Format.fprintf fmt "%a | %a" print_b a print_b b
    in
    Format.fprintf fmt "@[%a@]" print_b b
  in
  print_boolean fmt (bdd2boolean bdd)
