
open Asttypes
open Parsetree
open Types
open Typedtree
open Predef
open Printtyp

type error =
| Expression_type_clash of type_expr * type_expr
| Expression_type_wrong of type_expr * type_expr list
| Too_many_arguments
| Builtin_function of string
| Type_undefined of string

exception Error of error * Location.t

let check_subtype exp ty =
  if not (Btype.subtype exp.texp_type ty) then
    raise (Error (Expression_type_clash(exp.texp_type, ty), exp.texp_loc))

let rec type_ty ty =
  match ty.pty_desc with
  | Pty_name tname ->
    begin
      try Btype.type_of_string tname.txt (*XXX Should be implemented with a type environment *)
      with Not_found -> raise (Error (Type_undefined tname.txt, tname.loc))
    end
  | Pty_arrow (t,t') ->
    Btype.newty(Tarrow (type_ty t, type_ty t'))

and type_var env svar svalue =
  let loc = svar.pvar_loc in
  let add_ident ident ty loc env =
    let w () = Location.prerr_warning loc (Warnings.Unused_var ident) in
    try Typeenv.add_ident ~warn_unused:w ident ty env
    with Typeenv.Builtin_function ->
      raise (Error (Builtin_function ident, loc))
  in
  match svar.pvar_desc with
  | Pvar_ident v ->
    begin
      let value = type_expr env svalue in
      let ident = v.txt in
      let ty = value.texp_type in
      let env' = add_ident ident ty loc env in
      let var = {
        var_desc = Tvar_ident v;
        var_loc = loc;
        var_type = ty;
        var_env = env';
      } in
      (value, var, ident)
    end

  | Pvar_funct (f, args) ->
    begin
      let add_arg env (arg,ty) =
        let ident = arg.txt in
        try Typeenv.add_ident ident ty env
        with Typeenv.Builtin_function ->
          raise (Error (Builtin_function ident, loc)) in

      let arg_ty env (arg,t) =
        (arg,
         { ty_desc = type_ty t;
           ty_loc = t.pty_loc;
           ty_env = env; }) in

      let mk_type (arg,ty_desc) ty_res =
        Btype.newty(Tarrow(ty_desc, ty_res)) in

      let remove_arg (arg,_) env =
        Typeenv.remove_ident arg.txt env in

      let fid = f.txt in
      let args = List.map (arg_ty env) args in
      let argsty = List.map (fun (x,t) -> (x,t.ty_desc)) args in
      let env = List.fold_left add_arg env argsty in
      let value = type_expr env svalue in
      let env = List.fold_right remove_arg args env in
      let ty = List.fold_right mk_type argsty value.texp_type in
      let env = add_ident fid ty f.loc env in
      let var = {
        var_desc = Tvar_funct(f, args);
        var_loc = loc;
        var_type = ty;
        var_env = env;
      } in
      (value, var, fid)
    end


and type_expr env sexp =
  let loc = sexp.pexp_loc in
  match sexp.pexp_desc with
  | Pexp_boolconst s ->
    {
      texp_desc = Texp_boolconst s;
      texp_loc = loc;
      texp_type = Predef.type_bool;
      texp_env = env;
    }
  | Pexp_ident sid -> (* No id is declared as not found *)
    {
      texp_desc = Texp_ident sid;
      texp_loc = loc;
      texp_type = Typeenv.lookup_ident sid env;
      texp_env = env;
    }
  | Pexp_apply (sfunct, sargs) ->
    let funct = type_expr env sfunct in
    let (args, ty_res) = type_application env funct sargs in
    {
      texp_desc = Texp_apply(funct, args);
      texp_loc = loc;
      texp_type = ty_res;
      texp_env = env;
    }
  | Pexp_power (flag, sx, sy, sr) ->
    let x = type_expr env sx in
    check_subtype x type_rltl;
    let y = type_expr env sy in
    check_subtype y type_rltl;
    let r = match sr with
      | None -> None
      | Some sr' ->
        begin
          let r' = type_expr env sr' in
          check_subtype r' type_regex;
          Some r'
        end
    in
    {
      texp_desc = Texp_power (flag, x, y, r);
      texp_loc = loc;
      texp_type = type_rltl;
      texp_env = env;
    }
  | Pexp_overlap sexp ->
    let exp = type_expr env sexp in
    let exp_ty = exp.texp_type in
    let ty_expected = [type_regex; type_rltl] in
    if List.length (List.filter (fun ty -> exp_ty = ty) ty_expected) = 0 then
      raise (Error (Expression_type_wrong (exp_ty, ty_expected), exp.texp_loc))
    else
      {
        texp_desc = Texp_overlap exp;
        texp_loc = loc;
        texp_type = exp_ty;
        texp_env = env;
      }
  | Pexp_closure sexp ->
    let exp = type_expr env sexp in
    check_subtype exp type_regex;
    {
      texp_desc = Texp_closure exp;
      texp_loc = loc;
      texp_type = type_rltl;
      texp_env = env;
    }
  | Pexp_let ((svar, svalue), sbody) ->
    let value,var,ident = type_var env svar svalue in
    let body = type_expr var.var_env sbody in
    let env' = Typeenv.remove_ident ident var.var_env in
    {
      texp_desc = Texp_let ((var, value), body);
      texp_loc = loc;
      texp_type = body.texp_type;
      texp_env = env';
    }

and type_application env funct sargs =
  let check_args (args,ty) sarg =
    match ty.typ_desc with
    | Tvar _ -> raise (Error (Too_many_arguments, funct.texp_loc))
    | Tarrow (t, t') ->
      begin
        let arg = type_expr env sarg in
        check_subtype arg t;
        (arg::args, t')
      end
    | Tpoly (t,_,f) ->
      begin
        let arg = type_expr env sarg in
        check_subtype arg t;
        (arg::args, f arg.texp_type)
      end
  in
  let (args,ty_res) = List.fold_left check_args ([], funct.texp_type) sargs in
  (List.rev args,ty_res)

let expression env sexp = type_expr env sexp

let report_error ppf = function
  | Expression_type_clash (t1,t2) ->
    report_unification_error ppf (t1,[t2])
      (function ppf ->
        Format.fprintf ppf "This expression has type")
      (function ppf ->
        Format.fprintf ppf "but an expression was expected of type")
  | Expression_type_wrong (t1,t2) ->
    report_unification_error ppf (t1,t2)
      (function ppf ->
        Format.fprintf ppf "This expression has type")
      (function ppf ->
        Format.fprintf ppf "but an expression of the following types was expected:")
  | Too_many_arguments ->
      Format.fprintf ppf "This function is applied to too many arguments."
  | Builtin_function name ->
      Format.fprintf ppf "Cannot redefine buit-in operator %s" name
  | Type_undefined name ->
      Format.fprintf ppf "Type %s is undefined." name
