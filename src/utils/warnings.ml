
type t =
| Unused_var of string
| Generic of string

let is_active x = true

let message = function
  | Unused_var v -> "unused variable " ^ v ^ "."
  | Generic s -> s

let print ppf w =
  let msg = message w in
  Format.fprintf ppf ": %s" msg;
  Format.pp_print_flush ppf ()
