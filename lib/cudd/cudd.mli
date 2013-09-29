
type manager
type node

type tbool = False | True | Top

external create: unit -> manager =
  "caml_cudd_manager_make"

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

external bdd_cubes: node -> tbool array list =
  "caml_cudd_bdd_cubes"

(*external bdd_restricted_cubes: node -> tbool array list =
  "caml_cudd_bdd_restricted_cubes"*)

val print_minterm
  : (Format.formatter -> int -> unit) -> Format.formatter -> node -> unit

val fold_minterm:
  'a -> 'a -> (int -> 'a) -> ('a -> 'a) ->
  ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> node -> 'a

val bdd_print_wff: (int -> string) -> Format.formatter -> node -> unit
