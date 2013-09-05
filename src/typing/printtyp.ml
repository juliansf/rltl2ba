open Types

let rec print_type ppf t =
  match t.Types.typ_desc with
  | Tvar None -> ()
  | Tvar (Some v) -> Format.fprintf ppf "%s" v
  | Tarrow (t,t') ->
    Format.fprintf ppf "%a ->@;<1 2>%a" print_type t print_type t'
  | Tpoly (t,t',_) ->
    Format.fprintf ppf "(t < %a) ->@;<1 2>(t > %a)" print_type t print_type t'

let rec print_type_list ppf = function
  | [] -> ()
  | [t] -> Format.fprintf ppf "%a" print_type t
  | t::tl -> Format.fprintf ppf "%a or@;<1 2>%a"
    print_type t print_type_list tl

let report_unification_error ppf tr txt1 txt2 =
  let (t1, t2) = tr in
  Format.fprintf ppf
    "@[<v>\
      @[%t@;<1 2>%a@ \
        %t@;<1 2>%a\
      @]\
     @]"
    txt1 print_type t1
    txt2 print_type_list t2
