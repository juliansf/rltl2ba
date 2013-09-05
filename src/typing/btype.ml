
open Types

let new_id = ref (-1)

let newty desc =
  incr new_id; { typ_desc=desc; typ_id = !new_id }

let type_pairs = Hashtbl.create 8
let builtin_types = Hashtbl.create 8

let add_builtin name ty =
  Hashtbl.add builtin_types name ty

let type_of_string name =
  Hashtbl.find builtin_types name

let add_type_pair (t,t') =
  Hashtbl.add type_pairs (t,t') ()

let rec subtype ty ty' =
  if Hashtbl.mem type_pairs (ty, ty') then true
  else
    match ty.typ_desc, ty'.typ_desc with
    | Tvar s, Tvar s' when s=s' -> true
    | Tarrow (fty,aty), Tarrow (fty', aty') when subtype fty fty' ->
      subtype aty aty'
    | Tpoly (tin,tout,_), Tpoly (t'in,t'out,_) ->
      subtype tin t'in && subtype t'out tout
    | _ -> false
