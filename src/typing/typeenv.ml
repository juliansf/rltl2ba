
open Misc
(* Environment handling *)

exception Builtin_function

open Types

module EnvTbl =
struct
  type 'a state =
  | Builtin of 'a
  | Regular of 'a * bool ref * (unit -> unit) option

  type ('a,'b) t = ('a, 'b state) Hashtbl.t

  let empty () = Hashtbl.create 8

  let add ?warn_unused id x tbl =
    try
      match Hashtbl.find tbl id with
      | Builtin _ -> raise Builtin_function
      | Regular _ -> raise Not_found
    with
      Not_found ->
        Hashtbl.add tbl id (Regular (x, ref false, warn_unused))

  let add_builtin id x tbl =
    Hashtbl.add tbl id (Builtin x)

  let find id tbl =
    match Hashtbl.find tbl id with
    | Builtin data -> data
    | Regular (data,used,_) -> used := true; data

  let remove id tbl =
    match Hashtbl.find tbl id with
    | Builtin _ -> fatal_error "Env.EnvTab.remove"
    | Regular (d,u,w) -> Hashtbl.remove tbl id; (d,u,w)
end

type t = (Ident.t, type_expr) EnvTbl.t

let empty = EnvTbl.empty

(* Lookup by identifier *)
let lookup_ident id t =
  try EnvTbl.find id t
  with Not_found -> Predef.default_type

(* Insert by identifier *)
let add_ident ?warn_unused id ty t =
  EnvTbl.add ?warn_unused id ty t;
  t

(* Insert built-in function *)
let add_builtin id ty t =
  EnvTbl.add_builtin id ty t;
  t

(* Remove by identifier *)
let remove_ident id t =
  let (data, used, warn_unused) = EnvTbl.remove id t in
  if not !used then begin
    match warn_unused with
    | None -> ()
    | Some f -> f ()
  end;
  t

let initial =
  Predef.add_builtin_types ();
  Predef.build_type_pairs ();
  Predef.build_initial_env add_builtin (empty())
