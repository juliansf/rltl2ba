open Lexing

let absname = ref false

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool
}

type 'a loc = {
  txt : 'a;
  loc : t;
}

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let none = in_file "_none_"

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false;
}

let symbol_rloc s_start s_end = {
  loc_start = s_start;
  loc_end = s_end;
  loc_ghost = false;
}

let symbol_gloc s_start s_end = {
  loc_start = s_start;
  loc_end = s_end;
  loc_ghost = true;
}

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let mkloc txt loc = { txt; loc }
let mknoloc txt = mkloc txt none

let msg_file, msg_line, msg_chars, msg_to, msg_colon =
  ("File \"", "\", line ", ", characters ", "-", ":")

let absolute_path s =
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf filename =
  Format.fprintf ppf "%s" filename

let print_loc ppf loc =
  let file, line, startchar = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  begin
    Format.fprintf ppf "%s%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      Format.fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar
  end

let print ppf loc =
  Format.fprintf ppf "%a%s@." print_loc loc msg_colon

let print_error ppf loc =
  print ppf loc;
  Format.fprintf ppf "Error: "

let print_error_cur_file ppf = print_error ppf (in_file !input_name)

let print_warning loc ppf w =
  if Warnings.is_active w then begin
    print ppf loc;
    Format.fprintf ppf "Warning%a@." Warnings.print w
  end

let prerr_warning loc w = print_warning loc Format.err_formatter w
