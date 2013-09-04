
(* Error report *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print_error ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      Format.fprintf ppf "I/O error: %s" msg
  | Typecheck.Error (err, loc) ->
      Location.print_error ppf loc;
      Typecheck.report_error ppf err
  | e -> Format.fprintf ppf "@]"; raise e in

  Format.fprintf ppf "@[%a@]@." report exn
