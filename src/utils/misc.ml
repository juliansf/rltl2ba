
(* Errors *)
exception Fatal_error
exception ValOf_None
exception Not_implemented

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
    if not (Hashtbl.mem seen x) then (Hashtbl.add seen x (); true)
    else false
  ) xs

(* List comprehension *)
let rec range i j = if i > j then [] else i :: range (i+1) j

(* Statistics functions *)
let chrono f x =
  let t_start = Sys.time () in
  let r = f x in
  (Sys.time () -. t_start, r)

class timer =
object
  val mutable running = false
  val mutable _last = 0.0
  val mutable _acc = 0.0

  method start =
    running <- true;
    _last <- Sys.time ()

  method stop =
    if running then begin
      _acc <- _acc +. Sys.time() -. _last;
      running <- false
    end

  method reset =
    _last <- 0.0;
    _acc <- 0.0

  method value =
    if running then
      _acc +. Sys.time() -. _last
    else
      _acc
end

let human_readable_byte_count bytes =
  let unit = 1024. in
  if bytes < unit then Format.sprintf "%.0fB" bytes
  else
    let exp = int_of_float (log bytes /. log unit) in
    Format.sprintf "%.2f%sB" (bytes /. (unit ** (float_of_int exp)))
      (match exp-1 with
      | 0 -> "K" | 1 -> "M" | 2 -> "G"
      | 3 -> "T" | 4 -> "P" | 5 -> "E"
      | _ -> failwith "Ouch! This number is too big!")

(* Common modules *)
module IntSet = Set.Make(struct type t = int let compare = Pervasives.compare end)

module SetHash(S : Set.S) = struct
  type t = S.t
  let equal = S.equal
  let hash i = Hashtbl.hash (List.fast_sort Pervasives.compare (S.elements i))
end

module SetHashtbl(S : Set.S) = Hashtbl.Make(SetHash(S))
