
(* Errors *)
exception Fatal_error
exception ValOf_None

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

let valOf = function
  | Some x -> x
  | None -> raise ValOf_None

(* Hashtable functions *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
  tbl
