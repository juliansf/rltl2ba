(* Environment handling *)

open Types

exception Builtin_function

type t

val empty : unit -> t
val initial : t

(* Lookup by identifier *)
val lookup_ident: Ident.t -> t -> type_expr

(* Insert by identifier *)
val add_ident: ?warn_unused:(unit -> unit) -> Ident.t -> type_expr -> t -> t

(* Remove by identifier *)
val remove_ident: Ident.t -> t -> t

(* Insert built-in function *)
val add_builtin: Ident.t -> type_expr -> t -> t
