
module type Rltlba_options = sig
  val _annot : unit -> unit
  val _ahw : unit -> unit
  val _debug : int -> unit
  val _dintcode : unit -> unit
  val _dparsetree : unit -> unit
  val _dot : unit -> unit
  val _i : string -> unit
  val _nbw : unit -> unit
  val _nfa : unit -> unit
  val _o : string -> unit
  val _psl : unit -> unit
  val _rank : string -> unit
  val _s : string -> unit
(*  val _stdin : unit -> unit *)
  val _verbose : int -> unit
  val _version : unit -> unit
end

module type Arg_list = sig
  val list : (string * Arg.spec * string) list
end

module Make_rltlba_options (F : Rltlba_options) : Arg_list
