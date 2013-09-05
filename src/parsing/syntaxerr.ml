type error =
| Unclosed of Location.t * string * Location.t * string
| Expected of Location.t * string
| Other of Location.t

exception Error of error

let report_error ppf = function
  | Unclosed (opening_loc, opening, closing_loc, closing) ->
    begin
      Format.fprintf ppf "%aSyntax error: '%s' expected@."
        Location.print_error closing_loc closing;
      Format.fprintf ppf "%aThis '%s' might be unmatched"
        Location.print_error opening_loc opening
    end
  | Expected (loc, name) ->
    Format.fprintf ppf "%aSyntax error: '%s' expected@."
      Location.print_error loc name
  | Other loc ->
    Format.fprintf ppf "%aSyntax error" Location.print_error loc
