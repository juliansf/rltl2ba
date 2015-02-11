
val print_manager: Format.formatter -> Autmanager.t -> unit
val print_nfa: Autmanager.t -> Format.formatter -> Nfa.Make(Bool.Default.B).t -> unit
val nfa2dot: Autmanager.t -> Format.formatter -> Nfa.Make(Bool.Default.B).t -> unit
val print_ahw: Autmanager.t -> Format.formatter ->
  Ahw.Make(Nfa.Make(Bool.Default.B)).t -> unit
val ahw2dot: Autmanager.t -> Format.formatter ->
  Ahw.Make(Nfa.Make(Bool.Default.B)).t -> unit
