(* System configuration *)

let version = "0.1"

let rltl_suffix = ref ".rltl"
let psl_suffix = ref ".psl"
let reg_suffix = ref ".regex"

let print_config oc =
  let p name value = Printf.fprintf oc "%s: %s\n" name value in
  let p_bool name value = Printf.fprintf oc "%s: %B\n" name value in
  p "version" version;
  flush oc
