(* System configuration *)

val version: string
  (* The current version of the system *)

val rltl_suffix : string ref
  (* Suffix for RLTL files *)

val psl_suffix : string ref
  (* Suffix for PSL files *)

val reg_suffix : string ref
  (* Suffix for files containing regular expressions *)

val print_config : out_channel -> unit
