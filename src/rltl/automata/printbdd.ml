
let print_node_internal ?vars mgr fmt node =
  let vars = match vars with
    | Some v -> v
    | None -> Bdd.hashed_vars mgr in
  Cudd.print_minterm (fun fmt i ->
    Format.fprintf fmt "%s" (Hashtbl.find vars i)) fmt node

let print_node = print_node_internal ?vars:None

let string_of_node_internal ?vars mgr node =
  let vars = match vars with
    | Some v -> v
    | None -> Bdd.hashed_vars mgr in
  let _var i = Hashtbl.find vars i in
  let _not x = Format.sprintf "!%s" x in
  let _and x y =
    if x = "true" then y
    else if y = "true" then x
    else Format.sprintf "%s & %s" x y in
  let _or x y =
    if x = "false" then y
    else if y = "false" then x
    else Format.sprintf "%s | %s" x y in
  Cudd.fold_minterm "true" "false" _var _not _and _or node

let string_of_node = string_of_node_internal ?vars:None

let print_manager fmt mgr =
  let vars = Bdd.hashed_vars mgr in
  let f n b =
    Format.fprintf fmt "%d: %a\n" n
      (print_node_internal ?vars:(Some vars) mgr) b
  in
  Bdd.manager_apply f mgr

let print_vars fmt mgr =
  let vars = Bdd.hashed_vars mgr in
  Hashtbl.iter (fun i s -> Format.fprintf fmt "%d -> %s\n" i s) vars
