type level = int
type kind =
  | DEBUG
  | TRACE

(* Logging Kinds *)
let levels = Hashtbl.create 2

let enable k level =
  Hashtbl.replace levels k level

let disable k =
  Hashtbl.remove levels k

let is_enabled k =
  Hashtbl.mem levels k

let enable_all () =
  List.iter (fun k -> Hashtbl.replace levels k 0) [DEBUG; TRACE]

let level k =
  if Hashtbl.mem levels k then
    Hashtbl.find levels k
  else -1

let log ?(out=stderr) k l msg =
  if is_enabled k && (level k = 0 || level k = l) then
    Printf.fprintf out msg
  else
    Printf.ifprintf out msg

let debug ?(out=stderr) ?(level=0) msg =
  log ~out DEBUG level msg

let trace ?(out=stderr) ?(level=0) msg =
  log ~out TRACE level msg


let log_exec ?(out=stderr) k l f =
  if is_enabled k && (level k = 0 || level k = l) then f ()

let debug_exec ?(out=stderr) ?(level=0) f =
    log_exec ~out DEBUG level f

let trace_exec ?(out=stderr) ?(level=0) f =
    log_exec ~out TRACE level f
