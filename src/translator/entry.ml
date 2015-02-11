open Rltl

type t =
| ValEntry of Expgen.node
| FunEntry of (t -> t)

let val_entry = function
  | ValEntry x -> x
  | _ -> Misc.fatal_error "Entry.val_entry"

let fun_entry = function
  | FunEntry x -> x
  | _ -> Misc.fatal_error "Entry.fun_entry"
