
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


let uniques xs =
  let seen = Hashtbl.create (List.length xs) in
  List.filter (fun x ->
    let res = not (Hashtbl.mem seen x) in
    Hashtbl.replace seen x ();
    res
  ) xs
