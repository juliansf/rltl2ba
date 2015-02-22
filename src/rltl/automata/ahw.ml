module type S = sig
  type error =
  | Invalid_Ahw

  exception Error of error

  module Nfa : Nfa.S

  type state = int
  type trans = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_final: (state, unit) Hashtbl.t;
      ahw_stratum: (state, stratum) Hashtbl.t;
      ahw_strata: (stratum, strat_kind) Hashtbl.t;
      ahw_false: state;
      ahw_true: state;
    }

  type ahw = state

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
  val is_final: manager -> state -> bool
  val get_delta: manager -> state -> trans
  val get_stratum: manager -> state -> stratum
  val get_stratum_kind: manager -> stratum -> strat_kind
  val get_stratum_states: manager -> stratum -> state list
end


module Make(N : Nfa.S) =
struct

  type error =
  | Invalid_Ahw

  exception Error of error

  module Nfa = N
  module Label = Nfa.Label

  type state = int
  type trans = Nfa.Label.t
  type stratum = int
  type strat_kind = SAccept | SReject | SBuchi | SCoBuchi

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_final: (state, unit) Hashtbl.t;
      ahw_stratum: (state, stratum) Hashtbl.t;
      ahw_strata: (stratum, strat_kind) Hashtbl.t;
      ahw_false: state;
      ahw_true: state;
    }

  type ahw = state

  type t = ahw

  (* Initialization function *)
  let init bddmgr =
    let cuddmgr = bddmgr.Bdd.bdd_mgr in
    let delta = Hashtbl.create 8 in
    let final = Hashtbl.create 8 in
    let stratum = Hashtbl.create 8 in
    let strata = Hashtbl.create 8 in
    let _false = 0 in
    let _true = 1 in
    Hashtbl.add delta _false (Label.dfalse);
    Hashtbl.add stratum _false 0;
    Hashtbl.add strata 0 SReject;
    Hashtbl.add delta _true (Label.dtrue);
    Hashtbl.add stratum _true 1;
    Hashtbl.add strata 1 SAccept;

    {
      ahw_bddmgr = bddmgr;
      ahw_delta = delta;
      ahw_final = final;
      ahw_stratum = stratum;
      ahw_strata = strata;
      ahw_false = _false;
      ahw_true = _true;
    }

  (* Auxiliary functions *)
  let state_number = ref (1)
  let new_state () = incr state_number; !state_number
  let get_delta mgr q =
    try Hashtbl.find mgr.ahw_delta q
    with _ -> failwith ("ahw_of_nfa_exist_fuse_power"^string_of_int q)
  let set_delta mgr q succ = Hashtbl.add mgr.ahw_delta q succ

  let is_final mgr q =
    try Hashtbl.find mgr.ahw_final q; true
    with Not_found -> false
  let set_final mgr q = Hashtbl.add mgr.ahw_final q ()

  let stratum_number = ref (1)
  let new_stratum () = incr stratum_number; !stratum_number
  let get_stratum mgr q = Hashtbl.find mgr.ahw_stratum q
  let set_stratum mgr q h = Hashtbl.replace mgr.ahw_stratum q h

  let get_stratum_kind mgr h = Hashtbl.find mgr.ahw_strata h
  let set_stratum_kind mgr h k = Hashtbl.replace mgr.ahw_strata h k

  let get_stratum_states mgr h =
    Hashtbl.fold (fun q s qs -> if h = s then q::qs else qs) mgr.ahw_stratum []

  (** Simplifies the alternating automata *)
  (*XXX must be improved entirely!! *)
(*
  let simplify mgr ahw =
    let delta :(state,trans) Hashtbl.t= mgr.ahw_delta in
    (*let color = mgr.ahw_color in*)

    if Label.is_true (get_delta mgr ahw.ahw_regular) then
      Hashtbl.replace delta ahw.ahw_regular Label.dtrue
    else if Label.is_false (get_delta mgr ahw.ahw_regular) then
      Hashtbl.replace delta ahw.ahw_regular Label.dfalse;

    if Label.is_true (get_delta mgr ahw.ahw_dual) then
      Hashtbl.replace delta ahw.ahw_dual Label.dtrue
    else if Label.is_false (get_delta mgr ahw.ahw_dual) then
      Hashtbl.replace delta ahw.ahw_dual Label.dfalse;

    let rd = get_delta mgr ahw.ahw_regular in
    let dd = get_delta mgr ahw.ahw_dual in

    if rd != Label.dtrue &&
      rd != Label.dfalse &&
      dd != Label.dtrue &&
      dd != Label.dfalse
    then begin
      let del = Hashtbl.create 8 in
      let seen = Hashtbl.create 8 in
      let modified = Array.create (Hashtbl.length delta) true in
      let remove_spurious (q:state) (succ:trans) =
        if not (Hashtbl.mem seen q ||
                  q = ahw.ahw_regular || q = ahw.ahw_dual) then
          if Label.is_true succ then begin
            Hashtbl.add del q Label.dtrue;
            Hashtbl.add seen q ();
          end
          else if Label.is_false succ then begin
            Hashtbl.add del q Label.dfalse;
            Hashtbl.add seen q ();
          end
      (* if not (Hashtbl.mem seen q) && *)
      (*   not (q=init) && (Label.is_true succ || Label.is_false succ) then begin *)
      (*     Hashtbl.add del q succ; *)
      (*     Hashtbl.add seen q (); *)
      (*   end *)
      in
      Hashtbl.iter remove_spurious delta;

      while Hashtbl.length del > 0 do
      (* First pass - Remove spurious states in delta *)
        Hashtbl.iter (fun q succ ->
          let succ' =
            Hashtbl.fold (fun i b c ->
              Label.compose c b i
            ) del succ
          in
          Hashtbl.replace delta q (Label.simplify succ')
        ) delta;

        Hashtbl.clear del;

      (* Second pass - Recompute spurious states *)
        Hashtbl.iter remove_spurious delta
      done;

      if Label.is_true (get_delta mgr ahw.ahw_regular) then
        Hashtbl.replace delta ahw.ahw_regular Label.dtrue
      else if Label.is_false (get_delta mgr ahw.ahw_regular) then
        Hashtbl.replace delta ahw.ahw_regular Label.dfalse;

      if Label.is_true (get_delta mgr ahw.ahw_dual) then
        Hashtbl.replace delta ahw.ahw_dual Label.dtrue
      else if Label.is_false (get_delta mgr ahw.ahw_dual) then
        Hashtbl.replace delta ahw.ahw_dual Label.dfalse;
    end
*)



  let simplify2 mgr x =
    (*Printf.fprintf stderr "simplifying ahw node %d\n" x;*)
    let dep = Hashtbl.create 8 in
    let waiting = Queue.create () in

    Queue.add x waiting;


    (* Compute the dependency table *)
    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem dep x) then
        let dep_tbl = Hashtbl.create 2 in
        Label.iter_states (fun i -> Hashtbl.add dep_tbl i ()) (get_delta mgr x);
        Hashtbl.iter (fun i _ -> Queue.add i waiting) dep_tbl;
        (*let lst = Hashtbl.fold (fun i _ xs -> i::xs) dep_tbl [] in*)
        Hashtbl.add dep x dep_tbl;
    done;

    (* Check for those states that are true or false *)
    let sinks d =
      let t = Hashtbl.create 8 in
      Hashtbl.iter (fun i idep ->
        if Hashtbl.length idep = 0 then
          if Label.is_true (get_delta mgr i) then begin
            Hashtbl.add t i Label.dtrue;
            Hashtbl.remove d i;
          end
          else if Label.is_false (get_delta mgr i) then begin
            Hashtbl.add t i Label.dfalse;
            Hashtbl.remove d i;
          end) d;
      t
    in

    (* Remove states that are not needed *)
    let finished = ref false in
    while not !finished do
      let s = sinks dep in
      if Hashtbl.length s != 0 then
        Hashtbl.iter (fun i idep ->
          let idep_len = Hashtbl.length idep in
          Hashtbl.iter (fun j _ ->
            if Hashtbl.mem s j then Hashtbl.remove s j) idep;
          if Hashtbl.length idep < idep_len then
            set_delta mgr i (Label.map_states (fun k ->
              if Hashtbl.mem s k then
                Hashtbl.find s k
              else
                Label.dstate k) (get_delta mgr i))) dep
      else
        (*Printf.fprintf stderr "finished\n";*)
        finished := true
    done




  (** Builds the bottom/top specular pair *)
  let bottom mgr = mgr.ahw_false
  let top mgr = mgr.ahw_true

  (** Builds the accepting/rejecting specular pair of a letter. *)
  let letter mgr b =
    if Label.is_true b then top mgr
    else if Label.is_false b then bottom mgr
    else begin
      let q = new_state() in
      let h = new_stratum() in
      (* Set regular *)
      set_delta mgr q b;
      set_stratum mgr q h;
      set_stratum_kind mgr h SReject;
      q
    end

  (** Builds the disjunction of two AHWs *)
  let disj mgr x y =
    (* Printf.fprintf stderr "Disjunction... %!"; *)
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in
    if Label.is_true dx || Label.is_true dy then
      top mgr
    else if Label.is_false dx then
      y
    else if Label.is_false dy then
      x
    else begin
      (*Printf.fprintf stderr "(regular... %!";*)
      (* Create the new state for regular *)
      let q = new_state() in
      let h = new_stratum() in
      (* Build the transition *)
      set_delta mgr q (Label.simplify (Label.dor dx dy));
      set_stratum mgr q h;
      set_stratum_kind mgr h SReject;
      q
    end

  (** Builds the conjunction of two AHWs *)
  let conj mgr x y =
    (* Printf.fprintf stderr "Disjunction... %!"; *)
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in
    if Label.is_false dx || Label.is_false dy then
      top mgr
    else if Label.is_true dx then
      y
    else if Label.is_true dy then
      x
    else begin
      (*Printf.fprintf stderr "(regular... %!";*)
      (* Create the new state for regular *)
      let q = new_state() in
      let h = new_stratum() in
      (* Build the transition *)
      set_delta mgr q (Label.simplify (Label.dand dx dy));
      set_stratum mgr q h;
      set_stratum_kind mgr h SReject;
      q
    end

  (** Builds an AHW representing an existential NFA for fusion. *)
  let ahw_of_nfa_exist mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state () else mgr.ahw_true) in

    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_true then Label.dtrue
      else Label.dstate (state i)
    in

    (* Create the rejecting stratum *)
    let h = new_stratum() in
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
      if nfa_delta.(i) != [] then new_state () else mgr.ahw_false) in

    (* Create the states *)
    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_false then Label.dfalse
      else Label.dstate (state i)
    in

    (* Create the accepting stratum *)
    let h = new_stratum() in
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
    let dx = get_delta mgr x in

    if nfa = Nfa.nfa_false || Label.is_false dx then
      bottom mgr
    else begin
      let succ =
        if Label.is_true dx then Label.dtrue
        else Label.dstate x
      in
      let q = ahw_of_nfa_exist mgr nfa (fun _ -> succ) in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dor succ (get_delta mgr q));

      simplify2 mgr q;
      q
    end

  (** Builds the universal sequence of an NFA and an AHW *)
  let univ_concat mgr nfa x =
    let dx = get_delta mgr x in

    if nfa = Nfa.nfa_false || Label.is_true dx then
      top mgr
    else begin
      let succ =
        if Label.is_false dx then Label.dfalse
        else Label.dstate x
      in
      let q = ahw_of_nfa_univ mgr nfa (fun _ -> succ) in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then
        set_delta mgr q (Label.dand succ (get_delta mgr q));

      simplify2 mgr q;
      q
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

      simplify2 mgr q;
      q
    end

  (** Builds the universal sequence of an NFA and an AHW with overlap *)
  let univ_fusion mgr nfa x =
    let dx = get_delta mgr x in

    if nfa = Nfa.nfa_false || Label.is_true dx then
      top mgr
    else begin
      let succ =
        if Label.is_false dx then Label.dfalse
        else dx
      in
      let q = ahw_of_nfa_univ mgr nfa (fun _ -> succ) in

      simplify2 mgr q;
      q
    end

  let power mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_false dx then y
    else if Label.is_true dy then top mgr
    else begin
      (* Create the new state *)
      let q = new_state () in

      (* Build the transition *)
      let r' = ahw_of_nfa_exist mgr r (fun _ -> Label.dstate q) in

      (* Build and set successors for q *)
      let delta_q =
        Label.dor dy (Label.dand dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      q
    end

  let dual_power mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_true dx then y
    else if Label.is_false dy then bottom mgr
    else begin
      (* Create the new state *)
      let q = new_state () in

      (* Build the transition *)
      let r' = ahw_of_nfa_univ mgr r (fun _ -> Label.dstate q) in

      (* Build and set successors for q *)
      let delta_q =
        Label.dand dy (Label.dor dx (get_delta mgr r')) in
      set_delta mgr q delta_q;

      (* Add q to the new stratum *)
      set_stratum mgr q (get_stratum mgr r');

      q
    end

  let weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let q = power mgr x r y in

      (* Set its initial state as accepting *)
      set_final mgr q;

      (* Set the stratum as Buchi *)
      set_stratum_kind mgr (get_stratum mgr q) SBuchi;

      q
    end

  let dual_weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      (* Compute the automaton *)
      let q = dual_power mgr x r y in

      (* Set its initial state as rejecting *)
      set_final mgr q;

      (* Set the stratum as CoBuchi *)
      set_stratum_kind mgr (get_stratum mgr q) SCoBuchi;

      q
    end

  (** Builds an AHW representing an existential NFA for power fusion. *)
  let ahw_of_nfa_exist_fuse_power kind mgr nfa next =
    let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
    let n = Nfa.size nfa in
    let state_map = Array.init n (fun i ->
      if nfa_delta.(i) != [] then new_state () else mgr.ahw_true) in

    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_true then Label.dtrue
      else Label.dstate (state i)
    in

    (* Create a new stratum *)
    let h = new_stratum() in

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
      if nfa_delta.(i) != [] then new_state () else mgr.ahw_false) in

    let state i = state_map.(i) in
    let lstate i =
      let si = state i in
      if si = mgr.ahw_false then Label.dfalse
      else Label.dstate (state i)
    in

    (* Create a new stratum *)
    let h = new_stratum() in

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
      let q = new_state () in

      (* Build the transition *)
      let next d = Label.dor dy (Label.dand dy d) in
      let h, dr = ahw_of_nfa_exist_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set the new stratum as Rejecting *)
      set_stratum_kind mgr h SReject;

      q
    end

  let dual_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_true dx then y
    else if Label.is_false dy then bottom mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

      (* Build the transition *)
      let next d = Label.dand dy (Label.dor dy d) in
      let h, dr = ahw_of_nfa_univ_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next dr in
      set_delta mgr q delta_q;

      (* Set the new stratum as Accepting *)
      set_stratum_kind mgr h SAccept;

      q
    end

  let weak_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_false dx then y
    else if Label.is_true dy then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

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

      q
    end

  let dual_weak_power_fusion mgr x r y =
    let dx = get_delta mgr x in
    let dy = get_delta mgr y in

    if r = Nfa.nfa_false || Label.is_true dx then y
    else if Label.is_false dy then top mgr
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

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
        else new_state ()) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_true then Label.dtrue
        else if si = mgr.ahw_false then Label.dfalse
        else Label.dstate (state i)
      in

      (* Create the new accepting stratum *)
      let h = new_stratum() in
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
      state nfa_start
    end

  let dual_closure mgr nfa =
    if nfa = Nfa.nfa_false then top mgr
    else begin
      let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
      let n = Nfa.size nfa in

      let state_map = Array.init n (fun i ->
        if nfa_final.(i) then mgr.ahw_false
        else if nfa_delta.(i) = [] then mgr.ahw_true
        else new_state ()) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_false then Label.dfalse
        else if si = mgr.ahw_true then Label.dtrue
        else Label.dstate (state i)
      in

      (* Create the new rejecting stratum *)
      let h = new_stratum() in
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
      state nfa_start
    end
end
