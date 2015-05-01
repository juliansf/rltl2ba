module type S = sig
  type error =
  | Invalid_Ahw
  | Invalid_Ahw_Reference

  exception Error of error

  module Nfa : Nfa.S

  type state = int
  type reference = int
  type trans = Nfa.Label.t
  type label = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi
  type goodness = Good | Bad | Neutral

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_init: (reference, label) Hashtbl.t;
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
    }

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
  val pred: manager -> state -> Misc.IntSet.t
  val goodness: manager -> state -> goodness
end


module Make(N : Nfa.S) =
struct

  type error =
  | Invalid_Ahw
  | Invalid_Ahw_Reference

  exception Error of error

  module Nfa = N
  module Label = Nfa.Label
  module IntSet = Misc.IntSet


  type state = int
  type reference = int
  type trans = Nfa.Label.t
  type label = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi
  type goodness = Good | Bad | Neutral

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_init: (state, trans) Hashtbl.t;
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
    }

  type ahw = state

  type t = ahw

  (* Initialization function *)
  let init bddmgr =
    let cuddmgr = bddmgr.Bdd.bdd_mgr in
    let _size = Hashtbl.create 8 in
    let strata_size = Hashtbl.create 8 in
    let delta = Hashtbl.create 8 in
    let init = Hashtbl.create 8 in
    let pred = Hashtbl.create 8 in
    let final = Hashtbl.create 8 in
    let stratum = Hashtbl.create 8 in
    let strata = Hashtbl.create 8 in
    let simpl = Hashtbl.create 8 in
    let _false = 0 in
    let _true = 1 in
    Hashtbl.add delta _false Label.dfalse;
    Hashtbl.add init _false Label.dfalse;
    Hashtbl.add _size _false 1;
    Hashtbl.add stratum _false 0;
    Hashtbl.add strata 0 SReject;
    Hashtbl.add delta _true Label.dtrue;
    Hashtbl.add init _true Label.dtrue;
    Hashtbl.add _size _true 1;
    Hashtbl.add stratum _true 1;
    Hashtbl.add strata 1 SAccept;

    {
      ahw_bddmgr = bddmgr;
      ahw_delta = delta;
      ahw_init = init;
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

  (* Auxiliary functions *)
  let new_ref mgr = incr mgr.ahw_ref_number; !(mgr.ahw_ref_number)
  let get_init mgr r =
    try Hashtbl.find mgr.ahw_init r
    with Not_found -> raise (Error Invalid_Ahw_Reference)
  let set_init mgr r l = Hashtbl.add mgr.ahw_init r l

  let new_state mgr = incr mgr.ahw_state_number; !(mgr.ahw_state_number)
  let get_delta mgr q =
    try Hashtbl.find mgr.ahw_delta q
    with Not_found -> raise (Error Invalid_Ahw)
  let set_delta mgr q succ = Hashtbl.add mgr.ahw_delta q succ

  let is_final mgr q = Hashtbl.mem mgr.ahw_final q
  let set_final mgr q = Hashtbl.add mgr.ahw_final q ()

  let new_stratum mgr = incr mgr.ahw_stratum_number; !(mgr.ahw_stratum_number)
  let get_stratum mgr q = Hashtbl.find mgr.ahw_stratum q
  let set_stratum mgr q h = Hashtbl.replace mgr.ahw_stratum q h

  let get_stratum_kind mgr h = Hashtbl.find mgr.ahw_strata h
  let set_stratum_kind mgr h k = Hashtbl.replace mgr.ahw_strata h k

  let get_stratum_states mgr h =
    Hashtbl.fold (fun q s qs -> if h = s then q::qs else qs) mgr.ahw_stratum []

  let pred mgr q =
    try
      Hashtbl.fold (fun i _ s -> IntSet.add i s)
        (Hashtbl.find mgr.ahw_pred q) IntSet.empty
    with
      Not_found -> IntSet.empty (*raise (Error Invalid_Ahw)*)

  let goodness mgr q =
    match get_stratum_kind mgr (get_stratum mgr q) with
    | SAccept -> Good
    | SReject -> Bad
    | SBuchi -> if is_final mgr q then Good else Bad
    | SCoBuchi -> if is_final mgr q then Bad else Neutral

  let size mgr r =
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

  let reset_size mgr q = Hashtbl.remove mgr.ahw_size q
  let set_size mgr q n = Hashtbl.replace mgr.ahw_size q n
  let incr_size mgr q = Hashtbl.replace mgr.ahw_size q (size mgr q + 1)
  let decr_size mgr q = Hashtbl.replace mgr.ahw_size q (size mgr q - 1)

  let get_stratum_size mgr q h =
    if q = mgr.ahw_false || q = mgr.ahw_true then 1 else begin
      (if not (Hashtbl.mem mgr.ahw_strata_size q) then ignore (size mgr q));
      Hashtbl.find (Hashtbl.find mgr.ahw_strata_size q) h
    end

  let remove_state mgr q = begin
    Hashtbl.remove mgr.ahw_delta q;
    Hashtbl.remove mgr.ahw_final q;
    Hashtbl.remove mgr.ahw_stratum q;
    Hashtbl.remove mgr.ahw_simplified q;
    Hashtbl.remove mgr.ahw_pred q;
    Hashtbl.iter (fun _ t -> Hashtbl.remove t q) mgr.ahw_pred;
    Hashtbl.remove mgr.ahw_size q
  end

  let is_simplified mgr q = Hashtbl.mem mgr.ahw_simplified q
  let set_simplified mgr q = Hashtbl.add mgr.ahw_simplified q ()

  (** Simplifies the alternating automaton *)
  let simplify mgr x =
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
    done

  (*set_simplified mgr x*)

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
      set_stratum_kind mgr h SReject;
      set_init mgr r (Label.dstate q);
      r
    end

  (** Builds the disjunction of two AHWs *)
  let disj mgr x y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in
    if Label.is_true dx || Label.is_true dy then
      top mgr
    else if Label.is_false dx then
      y
    else if Label.is_false dy then
      x
    else begin
      let q = new_state mgr in
      let h = new_stratum mgr in
      (* Build the transition *)
      set_delta mgr q (Label.simplify (Label.dor dx dy));
      set_stratum mgr q h;
      set_stratum_kind mgr h SReject;
      simplify mgr q;
      q
    end

  (** Builds the disjunction of two AHWs *)
  let disj' mgr x y =
    let x' = get_init mgr x in
    let y' = get_init mgr y in
    if Label.is_true x' || Label.is_true y' then
      top mgr
    else if Label.is_false x' then
      y
    else if Label.is_false y' then
      x
    else begin
      let r = new_ref mgr in
      set_init mgr r (Label.dor x' y');
      r
    end
  let disj = disj'

  (** Builds the conjunction of two AHWs *)
  let conj mgr x y =
    (* Printf.fprintf stderr "Disjunction... %!"; *)
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in
    if Label.is_false dx || Label.is_false dy then
      bottom mgr
    else if Label.is_true dx then
      y
    else if Label.is_true dy then
      x
    else begin
      (*Printf.fprintf stderr "(regular... %!";*)
      (* Create the new state for regular *)
      let q = new_state mgr in
      let h = new_stratum mgr in
      (* Build the transition *)
      set_delta mgr q (Label.simplify (Label.dand dx dy));
      set_stratum mgr q h;
      set_stratum_kind mgr h SReject;
      simplify mgr q;
      q
    end

  (** Builds the conjunction of two AHWs *)
  let conj' mgr x y =
    let x' = get_init mgr x in
    let y' = get_init mgr y in
    if Label.is_false x' || Label.is_false y' then
      bottom mgr
    else if Label.is_true x' then
      y
    else if Label.is_true y' then
      x
    else begin
      let r = new_ref mgr in
      set_init mgr r (Label.dand x' y');
      r
    end
  let conj = conj'

  (** Builds an AHW representing an existential NFA for fusion. *)
  let ahw_of_nfa_exist mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_true) in

    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_true then Label.dtrue
      else Label.dstate (state i)
    in

    (* Create the rejecting stratum *)
    let h = new_stratum mgr in
    set_stratum_kind mgr h SReject;

    let next = next (state nfa_start) in

    (* Create the transitions and set the colors *)
    for i=0 to n-1 do
      if state i != mgr.ahw_true then begin
        let next_cond =
          List.fold_right (fun (b,j) s ->
            Label.dor (
              Label.dand b
                (if nfa_final.(j) then
                    if nfa_delta.(j) = [] then next
                    else Label.dor next (lstate j)
                 else lstate j)
            ) s
          ) nfa_delta.(i) Label.dfalse
        in
        set_delta mgr (state i) (Label.simplify next_cond);
        set_stratum mgr (state i) h
      end
    done;
    state nfa_start

  (** Builds an AHW representing an universal NFA for fusion. *)
  let ahw_of_nfa_univ mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_false) in

    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_false then Label.dfalse
      else Label.dstate (state i)
    in

    (* Create the accepting stratum *)
    let h = new_stratum mgr in
    set_stratum_kind mgr h SAccept;

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
        set_delta mgr (state i) (Label.simplify next_cond);
        set_stratum mgr (state i) h
      end
    done;
    state nfa_start

  (** Builds the existential sequence of an NFA and an AHW *)
  let concat mgr nfa x =
    let x' = get_init mgr x in

    if nfa = Nfa.nfa_false || Label.is_false x' then
      bottom mgr
    else begin
      let succ =
        if Label.is_true x' then Label.dtrue
        else x'
      in
      let q = ahw_of_nfa_exist mgr nfa (fun _ -> succ) in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dor succ (get_delta mgr q));

      simplify mgr q;
      let r = new_ref mgr in
      set_init mgr r (Label.dstate q);
      r
    end

  (** Builds the universal sequence of an NFA and an AHW *)
  let univ_concat mgr nfa x =
    let x' = get_init mgr x in

    if nfa = Nfa.nfa_false || Label.is_true x' then
      top mgr
    else begin
      let succ =
        if Label.is_false x' then Label.dfalse
        else x'
      in
      let q = ahw_of_nfa_univ mgr nfa (fun _ -> succ) in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dand succ (get_delta mgr q));

      simplify mgr q;
      let r = new_ref mgr in
      set_init mgr r (Label.dstate q);
      r
    end

  (* Builds the existential sequence of an NFA and an AHW with overlap *)
  let fusion mgr nfa x =
    let dx = get_delta mgr x in

    if nfa = Nfa.nfa_false || Label.is_false dx then
      bottom mgr
    else begin
      let succ =
        if Label.is_true dx then Label.dtrue
        else dx
      in
      let q = ahw_of_nfa_exist mgr nfa (fun _ -> succ) in

      simplify mgr q;
      q
    end

  (** Builds the universal sequence of an NFA and an AHW with overlap *)
  let univ_fusion mgr nfa x =
    try
    let dx = try get_delta mgr x with Not_found -> failwith "Ahw.univ_fusion.get_delta" in

    if nfa = Nfa.nfa_false || Label.is_true dx then
      top mgr
    else begin
      let succ =
        if Label.is_false dx then Label.dfalse
        else dx
      in
      let q = ahw_of_nfa_univ mgr nfa (fun _ -> succ) in

      simplify mgr q;
      q
    end
    with Not_found -> failwith "Ahw.univ_fusion"

  let power mgr x r y =
    let x' = get_init mgr x in
    let y' = get_init mgr y in

    if r = Nfa.nfa_false || Label.is_false x' then y
    else if Label.is_true y' then top mgr
    else begin
      (* Create the new state *)
      let q = new_state mgr in

      (* Build the transition *)
      let r' = ahw_of_nfa_exist mgr r (fun _ -> Label.dstate q) in

      (* x and y deltas *)
      let dx = Label.map_states (get_delta mgr) x' in
      let dy = Label.map_states (get_delta mgr) y' in

      (* Build and set successors for q *)
      let delta_q =
        Label.dor dy (Label.dand dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      simplify mgr q;

      (* Create the new reference for the initial condition *)
      let s = new_ref mgr in
      set_init mgr s (Label.dstate q);
      s
    end

  let dual_power mgr x r y =
    let x' = get_init mgr x in
    let y' = get_init mgr y in

    if r = Nfa.nfa_false || Label.is_true x' then y
    else if Label.is_false y' then bottom mgr
    else begin
      (* Create the new state *)
      let q = new_state mgr in

      (* Build the transition *)
      let r' = ahw_of_nfa_univ mgr r (fun _ -> Label.dstate q) in

      (* x and y deltas *)
      let dx = Label.map_states (get_delta mgr) x' in
      let dy = Label.map_states (get_delta mgr) y' in

      (* Build and set successors for q *)
      let delta_q =
        Label.dand dy (Label.dor dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      simplify mgr q;

      (* Create the new reference for the initial condition *)
      let s = new_ref mgr in
      set_init mgr s (Label.dstate q);
      s
    end

  let weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let s = power mgr x r y in

      (* Get the initial state *)
      let q = Label.get_state (get_init mgr s) in (* Fails if something went wrong *)

      (* Set its initial state as accepting *)
      set_final mgr q;

      (* Set the stratum as Buchi *)
      set_stratum_kind mgr (get_stratum mgr q) SBuchi;

      s
    end

  let dual_weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let s = dual_power mgr x r y in

      (* Get the initial state *)
      let q = Label.get_state (get_init mgr s) in (* Fails if something went wrong *)

      (* Set its initial state as rejecting *)
      set_final mgr q;

      (* Set the stratum as CoBuchi *)
      set_stratum_kind mgr (get_stratum mgr q) SCoBuchi;

      s
    end

  (** Builds an AHW representing an existential NFA for power fusion. *)
  let ahw_of_nfa_exist_fuse_power kind mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state mgr else mgr.ahw_true) in

    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_true then Label.dtrue
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
        set_delta mgr (state i) (Label.simplify next_cond);
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
      let si = state i in
      if si = mgr.ahw_false then Label.dfalse
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
        set_delta mgr (state i) (Label.simplify next_cond);
        set_stratum mgr (state i) h
      end
    done;
    h, start_trans

  let power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_false dx then y
    else if Label.is_true dy then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in

      (* Build the transition *)
      let next d = Label.dor dy (Label.dand dy d) in
      let h, dr = ahw_of_nfa_exist_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set the new stratum as Rejecting *)
      set_stratum_kind mgr h SReject;

      simplify mgr q;
      q
    end

  let dual_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_true dx then y
    else if Label.is_false dy then bottom mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in

      (* Build the transition *)
      let next d = Label.dand dy (Label.dor dy d) in
      let h, dr = ahw_of_nfa_univ_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set the new stratum as Accepting *)
      set_stratum_kind mgr h SAccept;

      simplify mgr q;
      q
    end

  let weak_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_false dx then y
    else if Label.is_true dy then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in

      (* Build the transition *)
      let next d = Label.dor dy (Label.dand dx d) in
      let h, dr = ahw_of_nfa_exist_fuse_power `Weak mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set its initial state as accepting *)
      set_final mgr q;

      (* Set the stratum as Buchi *)
      set_stratum_kind mgr h SBuchi;

      simplify mgr q;
      q
    end

  let dual_weak_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_true dx then y
    else if Label.is_false dy then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state mgr in

      (* Build the transition *)
      let next d = Label.dand dy (Label.dor dx d) in
      let h, dr = ahw_of_nfa_univ_fuse_power `Weak mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set its initial state as rejecting *)
      set_final mgr q;

      (* Set the stratum as CoBuchi *)
      set_stratum_kind mgr h SCoBuchi;

      simplify mgr q;
      q
    end

  let closure mgr nfa =
    if nfa = Nfa.nfa_false then bottom mgr
    else begin
      let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
      let n = Nfa.size nfa in

      let state_map = Array.init n (fun i ->
        if nfa_final.(i) then mgr.ahw_true
        else if nfa_delta.(i) = [] then mgr.ahw_false
        else new_state mgr) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_true then Label.dtrue
        else if si = mgr.ahw_false then Label.dfalse
        else Label.dstate (state i)
      in

      (* Create the new accepting stratum *)
      let h = new_stratum mgr in
      set_stratum_kind mgr h SAccept;

      (* Create the transitions and set the colors *)
      for i=0 to n-1 do
        if state i != mgr.ahw_true && state i != mgr.ahw_false then begin
          let succ =
            List.fold_right (fun (b,j) s ->
              Label.dor (Label.dand b (lstate j)) s
            ) nfa_delta.(i) Label.dfalse
          in
          set_delta mgr (state i) (Label.simplify succ);
          set_stratum mgr (state i) h
        end
      done;
      let q = state nfa_start in
      simplify mgr q;
      q
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
        else Label.dstate (state i)
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
          set_delta mgr (state i) succ; (*(Label.simplify succ)*);
          set_stratum mgr (state i) h
        end
      done;
      let q = state nfa_start in
      simplify mgr q;
      q
    end
end
