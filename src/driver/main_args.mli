
module type Rltlba_options = sig
  val _annot : unit -> unit
  val _dintcode : unit -> unit
  val _dparsetree : unit -> unit
  val _i : string -> unit
  val _o : string -> unit
  val _s : string -> unit
(*  val _stdin : unit -> unit *)
  val _verbose : int -> unit
  val _version : unit -> unit
end

module type Arg_list = sig
  val list : (string * Arg.spec * string) list
end

module Make_rltlba_options (F : Rltlba_options) : Arg_list
