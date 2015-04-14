
val print_manager: Format.formatter -> Autmanager.t -> unit
val print_nfa: Autmanager.t -> Format.formatter -> Nfa.Make(Bool.Default.B).t -> unit
val nfa2dot: Autmanager.t -> Format.formatter -> Nfa.Make(Bool.Default.B).t -> unit
val print_ahw: Autmanager.t -> Format.formatter ->
  Ahw.Make(Nfa.Make(Bool.Default.B)).t -> unit
val ahw2dot: Autmanager.t -> Format.formatter ->
  Ahw.Make(Nfa.Make(Bool.Default.B)).t -> unit
val print_nbw: Autmanager.t -> Format.formatter ->
  Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).t -> unit
val nbw2dot: Autmanager.t -> Format.formatter ->
  Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B))).t -> unit
