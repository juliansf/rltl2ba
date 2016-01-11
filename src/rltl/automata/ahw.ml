module type S = sig
  type error =
  | Invalid_Ahw
  | Invalid_Ahw_Reference
  | Invalid_Ahw_State
  | Invalid_Ahw_Stratum
  | Ahw_Reference_Exists of int

  exception Error of error

  module Nfa : Nfa.S

  type state = int
  type reference = int
  type trans = Nfa.Label.t
  type label = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi | STransient
  type goodness = Good | Bad | Neutral
(*
  type ref_info = {
    init: label;
    mutable links: int;
  }

  type state_info = {
    mutable delta: trans;
    mutable stratum: stratum;
    mutable link: int;
    mutable size: int;
    mutable is_final: bool;
    mutable is_simplified: bool;
    pred: (state, unit) Hashtbl.t;
  }
*)
  type manager (*=
    { ahw_bddmgr: Bdd.manager;
      ahw_states: (state, state_info) Hashtbl.t;
      ahw_link: (state, int) Hashtbl.t;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_ref: (reference, ref_info) Hashtbl.t;
      ahw_final: (state, unit) Hashtbl.t;
      ahw_stratum: (state, stratum) Hashtbl.t;
      ahw_strata: (stratum, strat_kind) Hashtbl.t;
      ahw_simplified: (state, unit) Hashtbl.t;
      ahw_pred: (state, (state, unit) Hashtbl.t) Hashtbl.t;
      ahw_ref_number: int ref;
      ahw_state_number: int ref;
      ahw_stratum_number: int ref;
      ahw_size: (state, int) Hashtbl.t;
      ahw_strata_size: (state, (stratum, int) Hashtbl.t) Hashtbl.t;
      ahw_false: state;
      ahw_true: state;
                 }*)

  type ahw = reference

  type t = ahw

  val init: Bdd.manager -> manager

  val bottom: manager -> t
  val top: manager -> t
  val letter: manager -> Nfa.label -> t
  val disj: manager -> t -> t -> t
  val conj: manager -> t -> t -> t
  val concat: manager -> Nfa.t -> t -> t
  val univ_concat: manager -> Nfa.t -> t -> t
  val fusion: manager -> Nfa.t -> t -> t
  val univ_fusion: manager -> Nfa.t -> t -> t
  val power: manager -> t -> Nfa.t -> t -> t
  val dual_power: manager -> t -> Nfa.t -> t -> t
  val weak_power: manager -> t -> Nfa.t -> t -> t
  val dual_weak_power: manager -> t -> Nfa.t -> t -> t
  val power_fusion: manager -> t -> Nfa.t -> t -> t
  val dual_power_fusion: manager -> t -> Nfa.t -> t -> t
  val weak_power_fusion: manager -> t -> Nfa.t -> t -> t
  val dual_weak_power_fusion: manager -> t -> Nfa.t -> t -> t
  val closure: manager -> Nfa.t -> t
  val dual_closure: manager -> Nfa.t -> t

  (*val simplify: manager -> t -> unit*)

  (*
    val univ_of_nfa: manager -> Nfa.t -> state * int list
    val exist_of_nfa: manager -> Nfa.t -> state * int list
  *)

  (* Auxiliary functions *)
  val size: manager -> t -> int
  val is_final: manager -> state -> bool
  val get_init: manager -> reference -> label
  val get_delta: manager -> state -> trans
  val get_stratum: manager -> state -> stratum
  val get_stratum_kind: manager -> stratum -> strat_kind
  val get_stratum_states: manager -> stratum -> state list
  val get_stratum_size: manager -> t -> stratum -> int
  val pred: manager -> reference -> (state -> Misc.IntSet.t)
  val goodness: manager -> state -> goodness
  val is_very_weak: manager -> state -> bool

  (* Memory Management functions *)
  val link_ref: manager -> reference -> unit
  val unlink_ref: manager -> reference -> unit
  val clean: ?keep_lastref:bool -> manager -> unit

  (* Printing functions *)
  val print_manager: manager -> unit
end


module Make(N : Nfa.S) =
struct

  type error =
  | Invalid_Ahw
  | Invalid_Ahw_Reference
  | Invalid_Ahw_State
  | Invalid_Ahw_Stratum
  | Ahw_Reference_Exists of int

  exception Error of error

  module Nfa = N
  module Label = Nfa.Label
  module IntSet = Misc.IntSet


  type state = int
  type reference = int
  type trans = Nfa.Label.t
  type label = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi | STransient
  type goodness = Good | Bad | Neutral

  type ref_info = {
    init: label;
    mutable rsize: int;
    mutable rlinks: int;
  }

  type state_info = {
    mutable delta: trans;
    mutable stratum: stratum;
    mutable links: int;
    mutable is_final: bool;
    mutable is_simplified: bool;
  }

  type stratum_info = {
    states: (state, unit) Hashtbl.t;
    mutable kind: strat_kind;
  }

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_state: (state, state_info) Hashtbl.t;
      ahw_ref: (reference, ref_info) Hashtbl.t;
      ahw_strata: (stratum, stratum_info) Hashtbl.t;

      ahw_ref_number: int ref;
      ahw_state_number: int ref;
      ahw_stratum_number: int ref;
      ahw_false: reference;
      ahw_true: reference;
    }

  type ahw = reference

  type t = ahw


  (* Printing functions *)
  let print_manager mgr =
    let kind i = match i.kind with
      | SAccept -> "A"
      | SReject -> "R"
      | SBuchi -> "B"
      | SCoBuchi -> "C"
      | STransient -> "T"
    in

    Printf.eprintf "### AHW Manager ########################################\n";
    (* REFERENCES *)
    Printf.eprintf "* References:\n";
    Hashtbl.iter (fun r i ->
        Printf.eprintf "** %d[%d]: %s\n" r i.rlinks (Label.to_string i.init)
      ) mgr.ahw_ref;

    Printf.eprintf "\n";

    Printf.eprintf "* Strata:\n";
    Hashtbl.iter (fun s i ->
        Printf.eprintf "** %d[%s]: { " s (kind i);
        Hashtbl.iter (fun q _ -> Printf.eprintf "%d " q) i.states;
        Printf.eprintf "}\n";
      ) mgr.ahw_strata;

    Printf.eprintf "\n";


    (* STATES *)
    Printf.eprintf "* States:\n";
    Hashtbl.iter (fun x i ->
        Printf.eprintf "** %d [links=%d; stratum=%d]: %s\n"
          x i.links i.stratum (Label.to_string i.delta)
      ) mgr.ahw_state;

    Printf.eprintf "########################################################\n"


  (* Auxiliary functions *)
  let ref_info mgr r =
    try Hashtbl.find mgr.ahw_ref r
    with Not_found -> raise (Error Invalid_Ahw_Reference)

  let state_info mgr q =
    try Hashtbl.find mgr.ahw_state q
    with Not_found -> (print_manager mgr; Printf.eprintf "State not found: %d\n" q; raise (Error Invalid_Ahw_State))

  let stratum_info mgr h =
    try Hashtbl.find mgr.ahw_strata h
    with Not_found -> (print_manager mgr; Printf.eprintf "Stratum not found: %d\n" h; raise (Error Invalid_Ahw_Stratum))

  let size mgr r =
    if r = mgr.ahw_false || r = mgr.ahw_true then 0
    else begin
      let rinfo = ref_info mgr r in
      if rinfo.rsize > 0 then rinfo.rsize
      else begin
        let visited = Hashtbl.create 8 in

        let rec count x =
          if Hashtbl.mem visited x then 0
          else begin
            Hashtbl.add visited x ();
            List.fold_left (fun c i -> c + count i)
              1 (Label.states (state_info mgr x).delta)
          end
        in
        let initials = Label.states rinfo.init in
        let size = List.fold_left (fun acc q -> acc + (count q)) 0 initials in
        rinfo.rsize <- size;
        size
      end
    end

  let new_ref mgr =
    incr mgr.ahw_ref_number;!(mgr.ahw_ref_number)

  let get_init mgr r =
    (ref_info mgr r).init

  let set_init mgr r l =
    if not (Hashtbl.mem mgr.ahw_ref r) then begin
      Hashtbl.add mgr.ahw_ref r {init=l; rsize=(-1); rlinks=0};
      ignore(size mgr r)
    end
    else
      raise (Error (Ahw_Reference_Exists r))

(*  let rec remove_state mgr q = begin
    let delta =
      if Hashtbl.mem mgr.ahw_delta q then
        Hashtbl.find mgr.ahw_delta q
      else
        Label.dtrue
    in
    Hashtbl.remove mgr.ahw_link q;
    Hashtbl.remove mgr.ahw_delta q;
    Hashtbl.remove mgr.ahw_final q;
    Hashtbl.remove mgr.ahw_stratum q;
    Hashtbl.remove mgr.ahw_simplified q;
    Hashtbl.remove mgr.ahw_pred q;
    Hashtbl.iter (fun _ t -> Hashtbl.remove t q) mgr.ahw_pred;
    Hashtbl.remove mgr.ahw_size q;

    Label.iter_states (unlink mgr) delta
  end*)
  let rec remove_state mgr q =
    let x = state_info mgr q in
    Label.iter_states (unlink mgr) x.delta;
    let hinfo = stratum_info mgr x.stratum in
    Hashtbl.remove hinfo.states q;
    Hashtbl.remove mgr.ahw_state q

(*  and link mgr q =
    if Hashtbl.mem mgr.ahw_link q then
      let l = Hashtbl.find mgr.ahw_link q in
      Hashtbl.replace mgr.ahw_link q (l+1)
    else
      Hashtbl.add mgr.ahw_link q 1
*)
  and link mgr q =
    let x = state_info mgr q in
      x.links <- x.links + 1

(*  and unlink mgr q =
    if Hashtbl.mem mgr.ahw_link q then
      let l = Hashtbl.find mgr.ahw_link q - 1 in
      if l > 0 then
        Hashtbl.replace mgr.ahw_link q l
      else
        remove_state mgr q
*)
  and unlink mgr q =
    let x = state_info mgr q in
    x.links <- x.links - 1;
    if x.links <= 0 then remove_state mgr q

  let links mgr q =
    (state_info mgr q).links

  let link_states mgr l =
    List.iter (link mgr) (Label.states l)

  let unlink_states mgr l =
    List.iter (unlink mgr) (Label.states l)

  let link_ref mgr r =
    Logger.debug ~level:10 "Linking %d.\n" r;
    let rinfo = ref_info mgr r in
    rinfo.rlinks <- rinfo.rlinks + 1

  let unlink_ref mgr r =
    Logger.debug ~level:10 "Unlinking %d.\n" r;
    let rinfo = ref_info mgr r in
    rinfo.rlinks <- rinfo.rlinks - 1

  let clean ?(keep_lastref=false) mgr =
    if Logger.level Logger.DEBUG = 100 then print_manager mgr;
    Logger.debug ~level:100 "keep_lastref=%B, last_ref=%d\n" keep_lastref (!(mgr.ahw_ref_number));
    Hashtbl.iter (fun r info ->
        Logger.debug ~level:100 "ref:%d -> {init:%s; size:%d; links:%d}\n" r (Label.to_string info.init) info.rsize info.rlinks;
        if info.rlinks <= 0 then
          if not keep_lastref || r != !(mgr.ahw_ref_number) then begin

            unlink_states mgr info.init;
            Hashtbl.remove mgr.ahw_ref r
          end
      ) mgr.ahw_ref;
    Hashtbl.iter (fun q info ->
        if info.links <= 0 then
          remove_state mgr q
      ) mgr.ahw_state;
    Hashtbl.iter (fun h info ->
        if Hashtbl.length info.states = 0 then
          Hashtbl.remove mgr.ahw_strata h
      ) mgr.ahw_strata;
    if Logger.level Logger.DEBUG = 100 then print_manager mgr

  (* Initialization function *)
(*  let init bddmgr =
    (*let cuddmgr = bddmgr.Bdd.bdd_mgr in*)
    let _size = Hashtbl.create 8 in
    let strata_size = Hashtbl.create 8 in
    let link = Hashtbl.create 8 in
    let delta = Hashtbl.create 8 in
    let ref_tbl = Hashtbl.create 8 in
    let pred = Hashtbl.create 8 in
    let final = Hashtbl.create 8 in
    let stratum = Hashtbl.create 8 in
    let strata = Hashtbl.create 8 in
    let simpl = Hashtbl.create 8 in
    let _false = 0 in
    let _true = 1 in
    Hashtbl.add delta _false Label.dfalse;
    Hashtbl.add ref_tbl _false {init=Label.dfalse;rlinks=1};
    Hashtbl.add _size _false 1;
    Hashtbl.add stratum _false 0;
    Hashtbl.add strata 0 STransient;
    Hashtbl.add delta _true Label.dtrue;
    Hashtbl.add ref_tbl _true {init=Label.dtrue;rlinks=1};
    Hashtbl.add _size _true 1;
    Hashtbl.add stratum _true 1;
    Hashtbl.add strata 1 STransient;

    {
      ahw_bddmgr = bddmgr;
      ahw_link = link;
      ahw_delta = delta;
      ahw_ref = ref_tbl;
      ahw_final = final;
      ahw_stratum = stratum;
      ahw_strata = strata;
      ahw_simplified = simpl;
      ahw_pred = pred;
      ahw_ref_number = ref (1);
      ahw_state_number = ref (1);
      ahw_stratum_number = ref (1);
      ahw_size = _size;
      ahw_strata_size = strata_size;
      ahw_false = _false;
      ahw_true = _true;
    }
*)

  let init bddmgr =
    (*let cuddmgr = bddmgr.Bdd.bdd_mgr in*)
    let ref_tbl = Hashtbl.create 8 in
    let states_tbl = Hashtbl.create 8 in
    let strata_tbl = Hashtbl.create 8 in
    let _false = 0 in
    let _true = 1 in

    let _info_false = {
      delta = Label.dfalse;
      stratum = 0;
      links = 1;
      is_final = false;
      is_simplified = true;
    } in
    Hashtbl.add states_tbl _false _info_false;
    Hashtbl.add ref_tbl _false {init=Label.dstate 0; rsize=1; rlinks=1};
    let _false_stratum = Hashtbl.create 1 in
    Hashtbl.add _false_stratum _false ();
    Hashtbl.add strata_tbl 0 {states=_false_stratum; kind=STransient};

    let _info_true = {
      delta = Label.dtrue;
      stratum = 1;
      links = 1;
      is_final = false;
      is_simplified = true;
    } in
    Hashtbl.add states_tbl _true _info_true;
    Hashtbl.add ref_tbl _true {init=Label.dstate 1; rsize=1; rlinks=1};
    let _true_stratum = Hashtbl.create 1 in
    Hashtbl.add _true_stratum _true ();
    Hashtbl.add strata_tbl 1 {states=_true_stratum; kind=STransient};

    {
      ahw_bddmgr = bddmgr;
      ahw_state = states_tbl;
      ahw_ref = ref_tbl;
      ahw_strata = strata_tbl;

      ahw_ref_number = ref (1);
      ahw_state_number = ref (1);
      ahw_stratum_number = ref (1);

      ahw_false = _false;
      ahw_true = _true;
    }


  let new_state mgr =
    incr mgr.ahw_state_number;
    let state = {
      delta = Label.dtrue;
      stratum = -1;
      links = 0;
      is_final = false;
      is_simplified = false;
    } in
    Hashtbl.add mgr.ahw_state (!(mgr.ahw_state_number)) state;
    !(mgr.ahw_state_number)

  let get_delta mgr q =
    (state_info mgr q).delta

  let set_delta mgr q succ =
    if Hashtbl.mem mgr.ahw_state q then
      (Hashtbl.find mgr.ahw_state q).delta <- succ
    else
      let state = {
        delta = succ;
        stratum = -1;
        links = 0;
        is_final = false;
        is_simplified = false;
      } in
      Hashtbl.add mgr.ahw_state q state

(*  let is_final mgr q =
    Hashtbl.mem mgr.ahw_final q
*)
  let is_final mgr q =
    (state_info mgr q).is_final

(*  let set_final mgr q =
    Hashtbl.add mgr.ahw_final q ()
*)
  let set_final mgr q =
    (state_info mgr q).is_final <- true

  let unset_final mgr q =
    (state_info mgr q).is_final <- false

  let new_stratum mgr =
    incr mgr.ahw_stratum_number; !(mgr.ahw_stratum_number)

(*  let get_stratum mgr q =
    Hashtbl.find mgr.ahw_stratum q
*)
  let get_stratum mgr q =
    (state_info mgr q).stratum

(*  let set_stratum mgr q h =
    Hashtbl.replace mgr.ahw_stratum q h
*)
  let set_stratum mgr q h =
    Logger.debug ~level:100 "Setting stratum %d for state %d.\n" h q;
    if not (Hashtbl.mem mgr.ahw_strata h) then
      Hashtbl.add mgr.ahw_strata h {states=Hashtbl.create 1; kind=STransient};

    let hinfo = stratum_info mgr h in
    let x = state_info mgr q in

    if (x.stratum != h) then begin
      if (x.stratum >= 0) then begin
        let xhinfo = stratum_info mgr x.stratum in
        Hashtbl.remove xhinfo.states q
      end;
      x.stratum <- h;
      Hashtbl.add hinfo.states q ()
    end


(*  let get_stratum_kind mgr h =
    Hashtbl.find mgr.ahw_strata h
*)
  let get_stratum_kind mgr h =
    (stratum_info mgr h).kind

(*  let set_stratum_kind mgr h k =
    Hashtbl.replace mgr.ahw_strata h k
*)
  let set_stratum_kind mgr h k =
    if (Hashtbl.mem mgr.ahw_strata h) then
      (Hashtbl.find mgr.ahw_strata h).kind <- k
    else
      let stratum = { states = Hashtbl.create 1; kind=k } in
      Hashtbl.add mgr.ahw_strata h stratum


(*  let get_stratum_states mgr h =
    Hashtbl.fold (fun q s qs -> if h = s then q::qs else qs) mgr.ahw_stratum []
*)
  let get_stratum_states mgr h =
    let hinfo = stratum_info mgr h in
    Hashtbl.fold (fun q _ qs -> q::qs) hinfo.states []

(*  let pred mgr q =
    try
      Hashtbl.fold (fun i _ s -> IntSet.add i s)
        (Hashtbl.find mgr.ahw_pred q) IntSet.empty
    with
      Not_found -> IntSet.empty (*raise (Error Invalid_Ahw)*)
*)
  let pred mgr r =
    let rinfo = (ref_info mgr r) in
    let reverse = Hashtbl.create 8 in
    let waiting = Queue.create () in
    let visited = Hashtbl.create 8 in

    (* Add initial states for processing *)
    Label.iter_states (fun i -> Queue.add i waiting) rinfo.init;

    (* Compute reverse table *)
    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem visited x) then begin
        Hashtbl.add visited x ();
        let xinfo = state_info mgr x in
        Label.iter_states (fun y ->
            let y_pred = if Hashtbl.mem reverse y then
                Hashtbl.find reverse y else IntSet.empty in
            Hashtbl.replace reverse y (IntSet.add x y_pred);
            Queue.add y waiting
        ) xinfo.delta
      end;
    done;

    (* Reverse function for reference [r] *)
    let rev q = try Hashtbl.find reverse q
      with Not_found -> raise (Error Invalid_Ahw_State) in
    rev

(*  let size mgr r =
    if r = mgr.ahw_false || r = mgr.ahw_true then 0
    else begin
      try Hashtbl.find mgr.ahw_size r
      with Not_found ->
        let strata = Hashtbl.create 8 in
        let visited = Hashtbl.create 8 in

        let incr_stratum_size h =
          let v = try Hashtbl.find strata h with Not_found -> 0 in
          Hashtbl.replace strata h (v+1)
        in
        let rec count x =
          if Hashtbl.mem visited x then 0
          else begin
            Hashtbl.add visited x ();
            incr_stratum_size (get_stratum mgr x);
            List.fold_left (fun c i -> c + count i)
              1 (Label.states (get_delta mgr x))
          end
        in
        let initials = Label.states (get_init mgr r) in
        let c = List.fold_left (fun acc q -> acc + (count q)) 0 initials in
        Hashtbl.replace mgr.ahw_size r c;
        Hashtbl.replace mgr.ahw_strata_size r strata;
        c
    end
*)
(*
  let reset_size mgr q = Hashtbl.remove mgr.ahw_size q
  let set_size mgr q n = Hashtbl.replace mgr.ahw_size q n
  let incr_size mgr q = Hashtbl.replace mgr.ahw_size q (size mgr q + 1)
  let decr_size mgr q = Hashtbl.replace mgr.ahw_size q (size mgr q - 1)
*)
(*let get_stratum_size mgr q h =
    (*Printf.eprintf "########################### get_stratum_size(%d)\n" h;*)
    if q = mgr.ahw_false || q = mgr.ahw_true then 1 else begin
      (if not (Hashtbl.mem mgr.ahw_strata_size q) then ignore (size mgr q));
      let ssize = Hashtbl.find (Hashtbl.find mgr.ahw_strata_size q) h in
      if ssize != List.length (get_stratum_states mgr h) then begin
        Printf.eprintf "########################### %d != %d\n"
          ssize (List.length (get_stratum_states mgr h));
        (*raise Exit*)
      end;
      ssize
    end
  *)
  (*let is_very_weak mgr*)
  let get_stratum_size mgr q h =
    let hinfo = stratum_info mgr h in
    let acc = ref 0 in
    Hashtbl.iter (fun q _ ->
        if links mgr q > 0 then incr acc
      ) hinfo.states;
    !acc

  let is_very_weak mgr r =
    let strata = Hashtbl.create (size mgr r) in
    let visited = Hashtbl.create 8 in

    let rec count x =
      if Hashtbl.mem visited x then ()
      else begin
        Hashtbl.add visited x ();
        let stratum = get_stratum mgr x in
        if Hashtbl.mem strata stratum then raise Exit;
        Hashtbl.add strata stratum ();
        List.iter count (Label.states(get_delta mgr x))
      end
    in
    try
      List.iter count (Label.states (get_init mgr r));
      true
    with Exit -> false;;

  let goodness mgr q =
    match get_stratum_kind mgr (get_stratum mgr q) with
    | SAccept -> Good
    | SReject -> Bad
    | SBuchi -> if is_final mgr q then Good else Bad
    | SCoBuchi -> if is_final mgr q then Bad else Neutral
    | STransient -> Good

  let is_simplified mgr q =
    (state_info mgr q).is_simplified
  let set_simplified mgr q =
    (state_info mgr q).is_simplified <- true
  let unset_simplified mgr q =
    (state_info mgr q).is_simplified <- false

  (** Simplifies the alternating automaton *)
  (*let simplify mgr x =
    let reverse = mgr.ahw_pred in
    let leaves = Hashtbl.create 8 in
    let waiting = Queue.create () in

    let get_reverse i =
      if not (Hashtbl.mem reverse i) then
        Hashtbl.add reverse i (Hashtbl.create 2);
      Hashtbl.find reverse i in

    (* Compute reverse transition relation and leaves *)
    Queue.add x waiting;
    while not (Queue.is_empty waiting) do
      let y = Queue.take waiting in
      if not (is_simplified mgr y) then begin
        set_simplified mgr y;
        if Label.is_true (get_delta mgr y) then
          Hashtbl.add leaves y Label.dtrue
        else if Label.is_false (get_delta mgr y) then
          Hashtbl.add leaves y Label.dfalse
        else
          Label.iter_states (fun i ->
            Hashtbl.add (get_reverse i) y ();
            if i != y then Queue.add i waiting
          ) (get_delta mgr y)
      end
    done;

    (* Delete leaves recursively *)
    let boundary = Hashtbl.create 8 in
    while Hashtbl.length leaves > 0 do
      let t = Hashtbl.copy leaves in
      Hashtbl.reset leaves;
      Hashtbl.iter (fun i d ->
        (*if i != x then remove_state mgr i;*)
        Hashtbl.iter (fun j _ ->
          if j != i then Hashtbl.add boundary j ()) (get_reverse i)
      ) t;
      Hashtbl.iter (fun q _->
        let mapi i = if Hashtbl.mem t i then Hashtbl.find t i else Label.dstate i in
        let delta = Label.map_states mapi (get_delta mgr q) in
        set_delta mgr q delta;
        if Label.is_true delta then
          Hashtbl.add leaves q Label.dtrue
        else if Label.is_false delta then
          Hashtbl.add leaves q Label.dfalse
      ) boundary;
      Hashtbl.reset boundary
    done*)

  (*set_simplified mgr x*)

  let simplify mgr x =
    let reverse = Hashtbl.create 8 in
    let leaves = Hashtbl.create 8 in
    let waiting = Queue.create () in

    let get_reverse i =
      if not (Hashtbl.mem reverse i) then
        Hashtbl.add reverse i (Hashtbl.create 2);
      Hashtbl.find reverse i in

    (* Compute reverse transition relation and leaves *)
    Queue.add x waiting;
    while not (Queue.is_empty waiting) do
      let y = Queue.take waiting in
      if not (is_simplified mgr y) then begin
        Logger.debug ~level:100 "Ahw.simplify: %d -> %s\n"
          y (Label.to_string (get_delta mgr y));
        set_simplified mgr y;
        if Label.is_true (get_delta mgr y) then
          Hashtbl.add leaves y Label.dtrue
        else if Label.is_false (get_delta mgr y) then
          Hashtbl.add leaves y Label.dfalse
        else
          Label.iter_states (fun i ->
            Hashtbl.add (get_reverse i) y ();
            if i != y then Queue.add i waiting
          ) (get_delta mgr y)
      end
    done;

    Logger.debug_exec ~level:100 begin fun _ ->
      Logger.debug ~level:100 "Ahw.simplify.leaves: [";
      Hashtbl.iter (fun x _ ->
          Logger.debug ~level:100 "%d, " x) leaves;
      Logger.debug ~level:100 "]\n";
    end;

    (* Delete leaves recursively *)
    let boundary = Hashtbl.create 8 in
    Logger.debug ~level:100 "Ahw.simplify.delete_leaves\n";
    while Hashtbl.length leaves > 0 do
      let t = Hashtbl.copy leaves in
      Hashtbl.reset leaves;
      Hashtbl.iter (fun i d ->
          (*if i != x then remove_state mgr i;*)
          Hashtbl.iter (fun j _ ->
              if j != i then Hashtbl.add boundary j ()) (get_reverse i)
        ) t;
      Hashtbl.iter (fun q _->
          let mapi i = if Hashtbl.mem t i then begin
              unlink mgr i;
              Hashtbl.find t i
            end
            else Label.dstate i in
          let delta = Label.map_states mapi (get_delta mgr q) in
          set_delta mgr q delta;
          if Label.is_true delta then
            Hashtbl.add leaves q Label.dtrue
          else if Label.is_false delta then
            Hashtbl.add leaves q Label.dfalse
        ) boundary;
      Hashtbl.reset boundary
    done;
    Logger.debug ~level:100 "Ahw.simplify.end\n"

  let simplify' mgr l =
    Label.iter_states (simplify mgr) l;
    let l' = Label.map_states (fun q ->
        let delta = get_delta mgr q in
        if Label.is_true delta then Label.dtrue
        else if Label.is_false delta then Label.dfalse
        else Label.dstate q
      ) l in
    if Label.is_true l' then Label.dstate mgr.ahw_true
    else if Label.is_false l' then Label.dstate mgr.ahw_false
    else l'

  let new_simplified_ref mgr q =
    simplify mgr q;

    Logger.debug ~level:100 "Ahw.new_simplified_ref(%d)\n" q;
    if Label.is_true (get_delta mgr q) then
      mgr.ahw_true
    else if Label.is_false (get_delta mgr q) then
      mgr.ahw_false
    else begin
      let r = new_ref mgr in
      link mgr q;
      set_init mgr r (Label.dstate q);
      r
    end

  (** Builds the bottom/top specular pair *)
  let bottom mgr = mgr.ahw_false
  let top mgr = mgr.ahw_true

  (** Builds the accepting/rejecting specular pair of a letter. *)
  let letter mgr b =
    if Label.is_true b then top mgr
    else if Label.is_false b then bottom mgr
    else begin
      let q = new_state mgr in
      let h = new_stratum mgr in
      let r = new_ref mgr in
      (* Set regular *)
      set_delta mgr q b;
      set_stratum mgr q h;
      set_stratum_kind mgr h STransient;
      set_init mgr r (Label.dstate q);
      link mgr q; (* State q is linked because it's an init state *)
      r
    end

  (** Builds the disjunction of two AHWs *)
  let disj mgr x y =
    if x = top mgr || y = top mgr then top mgr
    else if x = bottom mgr then y
    else if y = bottom mgr then x
    else begin
      let x' = get_init mgr x in
      let x'succ = Label.map_states (get_delta mgr) x' in
      let y' = get_init mgr y in
      let y'succ = Label.map_states (get_delta mgr) y' in
      let x'y'succ = Label.dor x'succ y'succ in

      if Label.is_true x'y'succ then
        top mgr
      else begin
        let l = Label.dor x' y' in
        let r = new_ref mgr in
        link_states mgr l;
        set_init mgr r l;
        r
      end
    end

  (** Builds the conjunction of two AHWs *)
  let conj mgr x y =
    if x = bottom mgr || y = bottom mgr then bottom mgr
    else if x = top mgr then y
    else if y = top mgr then x
    else begin
      let x' = get_init mgr x in
      let x'succ = Label.map_states (get_delta mgr) x' in
      let y' = get_init mgr y in
      let y'succ = Label.map_states (get_delta mgr) y' in
      let x'y'succ = Label.dand x'succ y'succ in

      if Label.is_false x'y'succ then
        bottom mgr
      else begin
        let l = Label.dand x' y' in
        let r = new_ref mgr in
        link_states mgr l;
        set_init mgr r l;
        r
      end
    end

  (** Builds an AHW representing an existential NFA for fusion. *)
  let ahw_of_nfa_exist mgr nfa stype next : state =
    let {Nfa.nfa_delta; nfa_start; nfa_final; nfa_scc} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
        if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_true) in
    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      if state i = mgr.ahw_true then Label.dtrue
      else Label.dstate (state i)
    in

    (* Create the new strata *)
    begin
      match stype with
      | `SingleStratum -> begin
          (* Create the rejecting stratum *)
          let h = new_stratum mgr in
          set_stratum_kind mgr h SReject;
          Array.iteri (fun i _ ->
              if state i != mgr.ahw_true then
                set_stratum mgr (state i) h
            ) nfa_delta;
        end
      | `MultiStrata -> begin
          Array.iter (fun states ->
              let h = new_stratum mgr in
              if Array.length states = 1 &&
                 not (List.exists (fun (_,s) ->
                     s=states.(0)) nfa_delta.(states.(0)))
              then
                set_stratum_kind mgr h STransient
              else
                set_stratum_kind mgr h SReject;

              Array.iter (fun i ->
                  if (state i) != mgr.ahw_true then
                    set_stratum mgr (state i) h
                ) states;
            ) nfa_scc;
        end;
    end;

    let next = next (state nfa_start) in
    (*let next'delta = Label.map_states (get_delta mgr) next in*)

    (* Create the transitions and set the colors *)
    for i=0 to n-1 do
      if state i != mgr.ahw_true then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
              Logger.debug ~level:101 "next_cond(%d,%d)\n" (state i) (state j);
            Label.dor (
              Label.dand b
                (if nfa_final.(j) then
                   if nfa_delta.(j) = [] then next
                   else if j = i then Label.dor next (lstate j)
                   else Label.dor next (lstate j)
                 else lstate j)
            ) s
          ) nfa_delta.(i) Label.dfalse
        in
        Logger.debug ~level:100 "Ahw.ahw_of_nfa_exists.next_cond = %s\n" (Label.to_string next_cond);
        Label.iter_states (link mgr) next_cond;
        set_delta mgr (state i) next_cond;
      end
    done;
    (*for i=0 to n-1 do
      if state i != mgr.ahw_true then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
              Logger.debug ~level:101 "next_cond(%d,%d)\n" (state i) (state j);
            Label.dor (
              Label.dand b (lstate j)
                (*if nfa_final.(j) then
                    if nfa_delta.(j) = [] then next
                    else Label.dor next (lstate j)
                 else lstate j*)
            ) s
          ) nfa_delta.(i) Label.dfalse
        in
        let next_cond = if nfa_final.(i) then
            Label.dor' next next_cond
          else next_cond in
        Label.iter_states (link mgr) next_cond;
        set_delta mgr (state i) next_cond;
      end
    done;*)
    state nfa_start

  (** Builds an AHW representing an universal NFA for fusion. *)
  let ahw_of_nfa_univ mgr nfa stype next : state =
    let {Nfa.nfa_delta; nfa_start; nfa_final; nfa_scc;} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_false) in

    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      if state i = mgr.ahw_false then Label.dfalse
      else Label.dstate (state i)
    in

    (* Create the stub states -- without useful info *)
    (*Array.iteri (fun i _ -> set_delta mgr (state i) Label.dfalse) nfa_delta;*)

    (* Create the new strata *)
    begin
      match stype with
      | `SingleStratum -> begin
          (* Create the accepting stratum *)
          let h = new_stratum mgr in
          set_stratum_kind mgr h SAccept;
          Array.iteri (fun i _ ->
              if state i != mgr.ahw_false then
                set_stratum mgr (state i) h
            ) nfa_delta;
        end
      | `MultiStrata -> begin
          Array.iter ( fun states ->
              let h = new_stratum mgr in
              if Array.length states = 1 &&
                 not (List.exists (fun (_,s) ->
                     s=states.(0)) nfa_delta.(states.(0)))
              then
                set_stratum_kind mgr h STransient
              else
                set_stratum_kind mgr h SAccept;

              Array.iter (fun i ->
                  if state i != mgr.ahw_false then
                    set_stratum mgr (state i) h
                ) states;
            ) nfa_scc;
        end;
    end;

    let next = next (state nfa_start) in

    (* Create the transitions and set the colors *)
    for i=0 to n-1 do
      if state i != mgr.ahw_false then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
            Label.dand (
              Label.dor (Label.dnot b)
                (if nfa_final.(j) then
                    if nfa_delta.(j) = [] then next
                    else Label.dand next (lstate j)
                 else lstate j)
            ) s
          ) nfa_delta.(i) Label.dtrue
        in
        Label.iter_states (link mgr) next_cond;
        set_delta mgr (state i) next_cond;
      end
    done;
    state nfa_start

  (** Builds the existential sequence of an NFA and an AHW *)
  let concat mgr nfa x =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in

    if nfa = Nfa.nfa_false || Label.is_false x'succ then
      bottom mgr
    else if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) && Label.is_true x'succ then
      top mgr
    else begin
      let succ =
        if Label.is_true x'succ then Label.dtrue
        else x'
      in
      let q = ahw_of_nfa_exist mgr nfa `MultiStrata (fun _ -> succ) in

      Logger.debug ~level:101 "Ahw.concat.succ = %s\n" (Label.to_string succ);
      (*if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dor' x'succ (get_delta mgr q));*)

      let r = new_simplified_ref mgr q in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        disj mgr x r
      else
        r
    end

  (** Builds the universal sequence of an NFA and an AHW *)
  let univ_concat mgr nfa x =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in

    if nfa = Nfa.nfa_false || Label.is_true x'succ then
      top mgr
    else begin
      let succ =
        if Label.is_false x'succ then Label.dfalse
        else x'
      in
      let q = ahw_of_nfa_univ mgr nfa `MultiStrata (fun _ -> succ) in

      (*if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dand succ (get_delta mgr q));*)

      let r = new_simplified_ref mgr q in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        conj mgr x r
      else
        r
    end

  (* Builds the existential sequence of an NFA and an AHW with overlap *)
  let fusion mgr nfa x =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in

    if nfa = Nfa.nfa_false || Label.is_false x'succ then
      bottom mgr
    else begin
      let succ =
        if Label.is_true x'succ then Label.dtrue
        else x'succ
      in
      let q = ahw_of_nfa_exist mgr nfa `MultiStrata (fun _ -> succ) in

      new_simplified_ref mgr q
    end

  (** Builds the universal sequence of an NFA and an AHW with overlap *)
  let univ_fusion mgr nfa x =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in

    if nfa = Nfa.nfa_false || Label.is_true x'succ then
      top mgr
    else begin
      let succ =
        if Label.is_false x'succ then Label.dfalse
        else x'succ
      in
      let q = ahw_of_nfa_univ mgr nfa `MultiStrata (fun _ -> succ) in

      new_simplified_ref mgr q
    end

  let power mgr x r y =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in
    let y' = get_init mgr y in
    let y'succ = Label.map_states (get_delta mgr) y' in

    if r = Nfa.nfa_false || Label.is_false x'succ then y
    else if Label.is_true y'succ then top mgr
    else begin
      (* Create the new state *)
      let q = new_state mgr in
      set_delta mgr q Label.dtrue; (* Stub delta. *)

      Logger.debug ~level:100 "Ahw.power.q = %d\n" q;

      (* Build the transition *)
      let r' =
        ahw_of_nfa_exist mgr r `SingleStratum (fun _ -> Label.dstate q) in

      Logger.debug ~level:100 "Ahw.power.3\n";
      (* x and y deltas *)
      let dx = Label.map_states (get_delta mgr) x' in
      let dy = Label.map_states (get_delta mgr) y' in
      Logger.debug ~level:100 "Ahw.power.4\n";
      (* Build and set successors for q *)
      let delta_q =
        Label.dor dy (Label.dand dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      Logger.debug ~level:100 "Ahw.power.5\n";
      let r = new_simplified_ref mgr q in
      Logger.debug ~level:100 "Ahw.power.6\n";
      if Logger.level Logger.DEBUG = 100 then print_manager mgr;
      r
    end

  let dual_power mgr x r y =
    let x' = get_init mgr x in
    let y' = get_init mgr y in

    if r = Nfa.nfa_false || Label.is_true x' then y
    else if Label.is_false y' then bottom mgr
    else begin
      (* Create the new state *)
      let q = new_state mgr in
      set_delta mgr q Label.dfalse; (* Stub delta. *)

      (* Build the transition *)
      let r' =
        ahw_of_nfa_univ mgr r `SingleStratum (fun _ -> Label.dstate q) in

      (* x and y deltas *)
      let dx = Label.map_states (get_delta mgr) x' in
      let dy = Label.map_states (get_delta mgr) y' in

      (* Build and set successors for q *)
      let delta_q =
        Label.dand dy (Label.dor dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;


      (*Printf.eprintf "^^^^^^ DUAL POWER'\nx=%s\ny=%s\nr=%s\n"
        (Label.to_string dx) (Label.to_string dy)
        (Label.to_string (get_delta mgr r'));
        Printf.eprintf "z=%s\n\n" (Label.to_string delta_q);*)

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      new_simplified_ref mgr q
    end

  let weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let r = power mgr x r y in

      if r = mgr.ahw_true || r = mgr.ahw_false then r
      else begin
        (* Get the initial state *)
        let q = Label.get_state (get_init mgr r) in
          (* Fails if something went wrong *)

        (* Set its initial state as accepting *)
        set_final mgr q;

        (* Set the stratum as Buchi *)
        set_stratum_kind mgr (get_stratum mgr q) SBuchi;

        r
      end
    end

  let dual_weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let r = dual_power mgr x r y in

      if r = mgr.ahw_true || r = mgr.ahw_false then r
      else begin
        (* Get the initial state *)
        let q = Label.get_state (get_init mgr r) in
          (* Fails if something went wrong *)

        (* Set its initial state as rejecting *)
        set_final mgr q;

        (* Set the stratum as CoBuchi *)
        set_stratum_kind mgr (get_stratum mgr q) SCoBuchi;

        r
      end
    end

  (** Builds an AHW representing an existential NFA for power fusion. *)
  let ahw_of_nfa_exist_fuse_power kind mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_true) in

    let state i = state_map.(i) in
    let lstate i =
      if state i = mgr.ahw_true then Label.dtrue
      else Label.dstate (state i)
    in

    (* Create a new stratum *)
    let h = new_stratum mgr in

    (* Tells if a state is final and have no successors *)
    let totally_final i = nfa_final.(i) && nfa_delta.(i) = [] in

    (* Compute the transition for the initial state *)
    let start_trans =
      List.fold_right (fun (b,j) s ->
        match kind with
        | `Weak ->
          begin
            if nfa_final.(j) then
              Label.dor b s
            else
              Label.dor (Label.dand b (lstate j)) s
          end
        | `Regular ->
          begin
            if totally_final j then
              Label.dor b s
            else
              Label.dor (Label.dand b (lstate j)) s
          end
      ) nfa_delta.(nfa_start) Label.dfalse
    in

    (* Compute next condition *)
    let next = next start_trans in

    (* Create the transitions and set the colors *)
    for i=0 to n-1 do
      if state i != mgr.ahw_true then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
            Label.dor (
              Label.dand b
                (if nfa_final.(j) then
                    if nfa_delta.(j) = [] then next
                    else
                      let next = (Label.compose next Label.dfalse (state j)) in
                      Label.dor next (lstate j)
                 else lstate j)
            ) s
          ) nfa_delta.(i) Label.dfalse
        in
        Label.iter_states (link mgr) next_cond;
        set_delta mgr (state i) next_cond;
        set_stratum mgr (state i) h
      end
    done;
    h, start_trans


(** Builds an AHW representing an universal NFA for power fusion. *)
  let ahw_of_nfa_univ_fuse_power kind mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_false) in

    let state i = state_map.(i) in
    let lstate i =
      if state i = mgr.ahw_false then Label.dfalse
      else Label.dstate (state i)
    in

    (* Create the stub states -- without useful info *)
    (*Array.iteri (fun i _ -> set_delta mgr (state i) Label.dfalse) nfa_delta;*)

    (* Create a new stratum *)
    let h = new_stratum mgr in

    (* Tells if a state is final and have no successors *)
    let totally_final i = nfa_final.(i) && nfa_delta.(i) = [] in

    (* Compute the transition for the initial state *)
    let start_trans =
      List.fold_right (fun (b,j) s ->
        match kind with
        | `Weak ->
          begin
            if nfa_final.(j) then
              Label.dand (Label.dnot b) s
            else
              Label.dand (Label.dor (Label.dnot b) (lstate j)) s
          end
        | `Regular ->
          begin
            if totally_final j then
              Label.dand (Label.dnot b) s
            else
              Label.dand (Label.dor (Label.dnot b) (lstate j)) s
          end
      ) nfa_delta.(nfa_start) Label.dtrue
    in

    (* Compute next condition *)
    let next = next start_trans in

    (* Create the transitions and set the colors *)
    for i=0 to n-1 do
      if state i != mgr.ahw_true then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
            Label.dand (
              Label.dor (Label.dnot b)
                (if nfa_final.(j) then
                    if nfa_delta.(j) = [] then next
                    else
                      let next = (Label.compose next Label.dtrue (state j)) in
                      Label.dand next (lstate j)
                 else lstate j)
            ) s
          ) nfa_delta.(i) Label.dtrue
        in
        Label.iter_states (link mgr) next_cond;
        set_delta mgr (state i) next_cond;
        set_stratum mgr (state i) h
      end
    done;
    h, start_trans

  let power_fusion mgr x r y =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in
    let y' = get_init mgr y in
    let y'succ = Label.map_states (get_delta mgr) y' in

    if r = Nfa.nfa_false || Label.is_false x'succ then y
    else if Label.is_true y'succ then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in
      set_delta mgr q Label.dtrue; (* Stub delta. *)

      (* Build the transition *)
      let next d = Label.dor y'succ (Label.dand x'succ d) in
      Logger.debug ~level:100 "next s = %s\n" (Label.to_string (next (Label.dstate 1)));
      let h, r'succ = ahw_of_nfa_exist_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next r'succ in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;

      (* Set the new stratum as Rejecting *)
      set_stratum_kind mgr h SReject;

      (* Add q to a new Transient stratum *)
      set_stratum mgr q (new_stratum mgr);

      new_simplified_ref mgr q
    end

  let dual_power_fusion mgr x r y =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in
    let y' = get_init mgr y in
    let y'succ = Label.map_states (get_delta mgr) y' in

    if r = Nfa.nfa_false || Label.is_true x'succ then y
    else if Label.is_false y'succ then bottom mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in
      set_delta mgr q Label.dfalse; (* Stub delta. *)

      (* Build the transition *)
      let next d = Label.dand y'succ (Label.dor x'succ d) in
      let h, r'succ = ahw_of_nfa_univ_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next r'succ in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;

      (* Set the new stratum as Accepting *)
      set_stratum_kind mgr h SAccept;

      (* Add q to a new Transient stratum *)
      set_stratum mgr q (new_stratum mgr);

      new_simplified_ref mgr q
    end

  let weak_power_fusion mgr x r y =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in
    let y' = get_init mgr y in
    let y'succ = Label.map_states (get_delta mgr) y' in

    if r = Nfa.nfa_false || Label.is_false x'succ then y
    else if Label.is_true y'succ then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in
      set_delta mgr q Label.dtrue; (* Stub delta. *)

      (* Build the transition *)
      let next d = Label.dor y'succ (Label.dand x'succ d) in
      let h, r'succ = ahw_of_nfa_exist_fuse_power `Weak mgr r next in

      (* Build and set successors for q *)
      let delta_q = next r'succ in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;

      (* Set its initial state as accepting *)
      set_final mgr q;

      (* Set the stratum as Buchi *)
      set_stratum_kind mgr h SBuchi;

      (* Add q to a new Transient stratum *)
      set_stratum mgr q (new_stratum mgr);

      new_simplified_ref mgr q
    end

  let dual_weak_power_fusion mgr x r y =
    let x' = get_init mgr x in
    let x'succ = Label.map_states (get_delta mgr) x' in
    let y' = get_init mgr y in
    let y'succ = Label.map_states (get_delta mgr) y' in

    if r = Nfa.nfa_false || Label.is_true x'succ then y
    else if Label.is_false y'succ then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in
      set_delta mgr q Label.dfalse; (* Stub delta. *)

      (* Build the transition *)
      let next d = Label.dand y'succ (Label.dor x'succ d) in
      let h, r'succ = ahw_of_nfa_univ_fuse_power `Weak mgr r next in

      (* Build and set successors for q *)
      let delta_q = next r'succ in
      set_delta mgr q delta_q;

      (* Link successors of q *)
      Label.iter_states (link mgr) delta_q;

      (* Set its initial state as rejecting *)
      set_final mgr q;

      (* Set the stratum as CoBuchi *)
      set_stratum_kind mgr h SCoBuchi;

      (* Add q to a new Transient stratum *)
      set_stratum mgr q (new_stratum mgr);

      new_simplified_ref mgr q
    end

  let closure mgr nfa =
    if nfa = Nfa.nfa_false then bottom mgr
    else begin
      let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
      let n = Nfa.size nfa in

      Logger.debug ~level:100 "Ahw.closure.1\n";

      let state_map = Array.init n (fun i ->
        if nfa_final.(i) then mgr.ahw_true
        else if nfa_delta.(i) = [] then mgr.ahw_false
        else new_state mgr) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_true then Label.dtrue
        else if si = mgr.ahw_false then Label.dfalse
        else Label.dstate si
      in
      Logger.debug ~level:100 "Ahw.closure.2\n";

      (* Create the new accepting stratum *)
      let h = new_stratum mgr in
      set_stratum_kind mgr h SAccept;

      Logger.debug ~level:100 "Ahw.closure.3\n";
      (* Create the transitions and set the colors *)
      for i=0 to n-1 do
        if state i != mgr.ahw_true && state i != mgr.ahw_false then begin
          let succ =
            List.fold_right (fun (b,j) s ->
              Label.dor (Label.dand b (lstate j)) s
            ) nfa_delta.(i) Label.dfalse
          in
          Label.iter_states (link mgr) succ;
          set_delta mgr (state i) succ;
          set_stratum mgr (state i) h
        end
      done;
      let q = state nfa_start in

      new_simplified_ref mgr q
    end

  let dual_closure mgr nfa =
    if nfa = Nfa.nfa_false then top mgr
    else begin
      let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
      let n = Nfa.size nfa in

      let state_map = Array.init n (fun i ->
        if nfa_final.(i) then mgr.ahw_false
        else if nfa_delta.(i) = [] then mgr.ahw_true
        else new_state mgr) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_false then Label.dfalse
        else if si = mgr.ahw_true then Label.dtrue
        else Label.dstate si
      in

      (* Create the new rejecting stratum *)
      let h = new_stratum mgr in
      set_stratum_kind mgr h SReject;

      (* Create the transitions and set the colors *)
      for i=0 to n-1 do
        if state i != mgr.ahw_true && state i != mgr.ahw_false then begin
          let succ =
            List.fold_right (fun (b,j) s ->
              Label.dand (Label.dor (Label.dnot b) (lstate j)) s
            ) nfa_delta.(i) Label.dtrue
          in
          Label.iter_states (link mgr) succ;
          set_delta mgr (state i) succ;
          set_stratum mgr (state i) h
        end
      done;
      let q = state nfa_start in

      new_simplified_ref mgr q
    end

end
