
let mk_annot f =
  "-annot", Arg.Unit f,
  " Print the parsed expression with type information"
;;

let mk_dintcode f =
  "-dintcode", Arg.Unit f,
  " Dump the internal representation of the parsed expression"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f,
  " Dump the parse tree of the parsed expression"
;;

let mk_dot f =
  "-dot", Arg.Unit f,
  "Output the generated automata in DOT format."
;;

let mk_i f =
  "-i", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

let mk_nfa f =
  "-nfa", Arg.Unit f,
  " Output the generated NFA automata of the parsed regular expression."
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set the output to <file>"
;;

let mk_s f =
  "-s", Arg.String f, "<expr>  Treat <expr> as an expression"

let mk_stdin f =
  "-stdin", Arg.Unit f, " Read the expresion from standard input"
;;

let mk_verbose f =
  "-verbose", Arg.Int f, "<level>  Sets verbosity to <level>"

let mk_version f =
  "-version", Arg.Unit f, " Print version number and exit"
;;

module type Rltlba_options = sig
  val _annot : unit -> unit
  val _dintcode : unit -> unit
  val _dparsetree : unit -> unit
  val _dot : unit -> unit
  val _i : string -> unit
  val _nfa : unit -> unit
  val _o : string -> unit
  val _s : string -> unit
(*  val _stdin : unit -> unit *)
  val _verbose : int -> unit
  val _version : unit -> unit
end

module type Arg_list = sig
  val list : (string * Arg.spec * string) list
end

module Make_rltlba_options (F : Rltlba_options) =
struct
  let list = [
    mk_annot F._annot;
    mk_dintcode F._dintcode;
    mk_dparsetree F._dparsetree;
    mk_dot F._dot;
    mk_i F._i;
    mk_nfa F._nfa;
    mk_o F._o;
    mk_s F._s;
(*    mk_stdin F._stdin; *)
    mk_verbose F._verbose;
    mk_version F._version;
  ]
end
