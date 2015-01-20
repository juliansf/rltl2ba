module type S = sig
  type error =
  | Invalid_Ahw

  exception Error of error

  module Nfa : Nfa.S

  type state = int
  type trans = Nfa.Label.t

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_color: (state, int) Hashtbl.t;
      ahw_false: state;
      ahw_true: state;
    }

  type ahw = state

  type t =
    {ahw_regular: ahw;
     ahw_dual: ahw;
    }

  val init: Bdd.manager -> manager

  val empty: manager -> t
  val letter: manager -> Nfa.label -> t
  val negate: t -> t
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

  (*val simplify: manager -> t -> unit*)

  (*
    val univ_of_nfa: manager -> Nfa.t -> state * int list
    val exist_of_nfa: manager -> Nfa.t -> state * int list
  *)

  (* Auxiliary functions *)
  val get_delta: manager -> state -> trans
  val get_color: manager -> state -> int
end


module Make(N : Nfa.S) =
struct

  type error =
  | Invalid_Ahw

  exception Error of error

  module Nfa = N
  module Label = Nfa.Label

  type state = int
  type trans = Label.t

  type manager =
    { ahw_bddmgr: Bdd.manager;
      ahw_delta: (state, trans) Hashtbl.t;
      ahw_color: (state, int) Hashtbl.t;
      ahw_false: state;
      ahw_true: state;
    }

  type ahw = state

  type t =
    {ahw_regular: ahw;
     ahw_dual: ahw;
    }

  (* Inisialization function *)
  let init bddmgr =
    let cuddmgr = bddmgr.Bdd.bdd_mgr in
    let delta = Hashtbl.create 8 in
    let color = Hashtbl.create 8 in
    let _false = 0 in
    let _true = 1 in
    Hashtbl.add delta _false (Label.dfalse);
    Hashtbl.add color _false 0;
    Hashtbl.add delta _true (Label.dtrue);
    Hashtbl.add color _true 0;
    {
      ahw_bddmgr = bddmgr;
      ahw_delta = delta;
      ahw_color = color;
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
  let get_color mgr q = Hashtbl.find mgr.ahw_color q
  let set_color mgr q c = Hashtbl.add mgr.ahw_color q c

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
    Printf.fprintf stderr "simplifying ahw node %d\n" x;
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
        Printf.fprintf stderr "finished\n";
        finished := true
    done




  (** Builds the bottom/top specular pair *)
  let empty mgr =
    { ahw_regular=mgr.ahw_false; ahw_dual=mgr.ahw_true }

  (** Flips the specular pair *)
  let negate {ahw_regular=regular; ahw_dual=dual} =
    {ahw_regular=dual; ahw_dual=regular;}

  (** Builds the accepting/rejecting specular pair of a letter. *)
  let letter mgr b =
    if Label.is_true b then negate (empty mgr)
    else if Label.is_false b then empty mgr
    else begin
      let q,q' = new_state(), new_state() in
      (* Set regular *)
      set_delta mgr q b;
      set_color mgr q 0;
      (* Set dual *)
      set_delta mgr q' (Label.dnot b);
      set_color mgr q' 0;

      { ahw_regular=q; ahw_dual=q'; }
    end

  (** Builds the disjunction and its dual of two AHWs *)
  let disj mgr pair1 pair2 =
    (* Printf.fprintf stderr "Disjunction... %!"; *)
    let dp1r = get_delta mgr pair1.ahw_regular in
    let dp2r = get_delta mgr pair2.ahw_regular in
    if Label.is_true dp1r || Label.is_true dp2r then
      negate (empty mgr)
    else if Label.is_false dp1r then
      pair2
    else if Label.is_false dp2r then
      pair1
    else begin
      (*Printf.fprintf stderr "(regular... %!";*)
      (* Create the new state for regular *)
      let q = new_state () in
      (* Build the transition *)
      let succ1 = get_delta mgr pair1.ahw_regular in
      let succ2 = get_delta mgr pair2.ahw_regular in
      set_delta mgr q (Label.simplify (Label.dor succ1 succ2));
      set_color mgr q 1;
      (*Printf.fprintf stderr "done) %!";*)

      (* Create the new state for dual *)
      (*Printf.fprintf stderr "(dual... %!";*)
      let q' = new_state () in
      (* Build the transition *)
      let succ1' = get_delta mgr pair1.ahw_dual in
      let succ2' = get_delta mgr pair2.ahw_dual in
      set_delta mgr q' (Label.simplify (Label.dand succ1' succ2'));
      set_color mgr q' 1;
      (*Printf.fprintf stderr "done) %!";*)
      (*
      simplify mgr q;
      simplify mgr q';
    *)
      (* Build the specular pair *)
      (*Printf.fprintf stderr " done\n%!";*)
      { ahw_regular = q; ahw_dual = q' }
    end

  (** Builds the conjunction and its dual of two AHWs *)
  let conj mgr pair1 pair2 =
    negate (disj mgr (negate pair1) (negate pair2))


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
        set_color mgr (state i) 1
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
        set_color mgr (state i) 0
      end
    done;
    state nfa_start

  (** Builds the existential sequence of an NFA and an AHW *)
  let concat mgr nfa pair =
    let succ_pr = get_delta mgr pair.ahw_regular in
    let succ_pd = get_delta mgr pair.ahw_dual in

    if nfa = Nfa.nfa_false || Label.is_false succ_pr then empty mgr
    else begin
      let succpr,succpd =
        if Label.is_true succ_pr then
          Label.dtrue, Label.dfalse
        else Label.dstate pair.ahw_regular, Label.dstate pair.ahw_dual
      in
      let regular = ahw_of_nfa_exist mgr nfa (fun _ -> succpr) in
      let dual = ahw_of_nfa_univ mgr nfa (fun _ -> succpd) in

      if nfa.Nfa.nfa_final.(nfa.Nfa.nfa_start) then begin
        set_delta mgr regular (Label.dor succ_pr (get_delta mgr regular));
        set_delta mgr dual (Label.dand succ_pd (get_delta mgr dual))
      end;

      simplify2 mgr regular;
      simplify2 mgr dual;

      {
        ahw_regular = regular;
        ahw_dual = dual;
      }
    end

  (** Builds the universal sequence of an NFA and an AHW *)
  let univ_concat mgr nfa pair =
    negate (concat mgr nfa (negate pair))

  (* Builds the existential sequence of an NFA and an AHW with overlap *)
  let fusion mgr nfa pair =
    let succ_pr = get_delta mgr pair.ahw_regular in
    let succ_pd = get_delta mgr pair.ahw_dual in

    if nfa = Nfa.nfa_false || Label.is_false succ_pr then empty mgr
    else begin
      let succ_pr,succ_pd =
        if Label.is_true succ_pr then
          Label.dtrue, Label.dfalse
        else succ_pr,succ_pd
      in
      let regular = ahw_of_nfa_exist mgr nfa (fun _ -> succ_pr) in
      let dual = ahw_of_nfa_univ mgr nfa (fun _ -> succ_pd) in

      simplify2 mgr regular;
      simplify2 mgr dual;

      {
        ahw_regular = regular;
        ahw_dual = dual;
      }
    end

  (** Builds the universal sequence of an NFA and an AHW with overlap *)
  let univ_fusion mgr nfa pair =
    negate (fusion mgr nfa (negate pair))

  let power mgr x r y =
    let succ_xr = get_delta mgr x.ahw_regular in
    let succ_xd = get_delta mgr x.ahw_dual in
    let succ_yr = get_delta mgr y.ahw_regular in
    let succ_yd = get_delta mgr y.ahw_dual in

    if r = Nfa.nfa_false || Label.is_false succ_xr then y
    else if Label.is_true succ_yr then negate (empty mgr)
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

      (* Build the transition *)
      let r' = ahw_of_nfa_exist mgr r (fun _ -> Label.dstate q) in

      (* Build and set successors for q *)
      let delta_q =
        Label.dor succ_yr (Label.dand succ_xr (get_delta mgr r')) in
      set_delta mgr q delta_q;
      set_color mgr q 1;

      (* Create the new state for dual *)
      let q' = new_state () in

      (* Build the transition *)
      let r' = ahw_of_nfa_univ mgr r (fun _ -> Label.dstate q') in

      (* Build and set successors for q' *)
      let delta_q' =
        Label.dand succ_yd (Label.dor succ_xd (get_delta mgr r')) in
      set_delta mgr q' delta_q';
      set_color mgr q' 0;

      (* Build the specular pair *)
      { ahw_regular = q; ahw_dual = q' }
    end


  let dual_power mgr x r y =
    negate (power mgr (negate x) r (negate y))

  let weak_power mgr x r y =
    if r.Nfa.nfa_final.(r.Nfa.nfa_start) then disj mgr x y
    else begin
      let {ahw_regular; ahw_dual} = power mgr x r y in
      set_color mgr ahw_regular 2;
      set_color mgr ahw_dual 1;
      { ahw_regular; ahw_dual }
    end

  let dual_weak_power mgr x r y =
    negate (weak_power mgr (negate x) r (negate y))



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
        set_color mgr (state i) 1
      end
    done;
    start_trans


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
        set_color mgr (state i) 0
      end
    done;
    start_trans

  let power_fusion mgr x r y =
    let succ_xr = get_delta mgr x.ahw_regular in
    let succ_xd = get_delta mgr x.ahw_dual in
    let succ_yr = get_delta mgr y.ahw_regular in
    let succ_yd = get_delta mgr y.ahw_dual in

    if r = Nfa.nfa_false || Label.is_false succ_xr then y
    else if Label.is_true succ_yr then negate (empty mgr)
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

      (* Build the transition *)
      let next d = Label.dor succ_yr (Label.dand succ_xr d) in
      let succ_r = ahw_of_nfa_exist_fuse_power `Regular mgr r next in

      (* Build and set successors for q *)
      let delta_q = next succ_r in
      set_delta mgr q delta_q;
      set_color mgr q 1;

      (* Create the new state for dual *)
      let q' = new_state () in

      (* Build the transition *)
      let next d = Label.dand succ_yd (Label.dor succ_xd d) in
      let succ_r' = ahw_of_nfa_univ_fuse_power `Regular mgr r next in

      (* Build and set successors for q' *)
      let delta_q' = next succ_r'  in
      set_delta mgr q' delta_q';
      set_color mgr q' 0;

      (* Build the specular pair *)
      { ahw_regular = q; ahw_dual = q' }
    end

  let dual_power_fusion mgr x r y =
    negate (power_fusion mgr (negate x) r (negate y))

  let weak_power_fusion mgr x r y =
    let succ_xr = get_delta mgr x.ahw_regular in
    let succ_xd = get_delta mgr x.ahw_dual in
    let succ_yr = get_delta mgr y.ahw_regular in
    let succ_yd = get_delta mgr y.ahw_dual in

    if r = Nfa.nfa_false || Label.is_false succ_xr then y
    else if Label.is_true succ_yr then negate (empty mgr)
    else begin
      (* Create the new state for regular *)
      let q = new_state () in

      (* Build the transition *)
      let next d = Label.dor succ_yr (Label.dand succ_xr d) in
      let succ_r = ahw_of_nfa_exist_fuse_power `Weak mgr r next in

      (* Build and set successors for q *)
      let delta_q = next succ_r in
      set_delta mgr q delta_q;
      set_color mgr q 1;

      (* Create the new state for dual *)
      let q' = new_state () in

      (* Build the transition *)
      let next d = Label.dand succ_yd (Label.dor succ_xd d) in
      let succ_r' = ahw_of_nfa_univ_fuse_power `Weak mgr r next in

      (* Build and set successors for q' *)
      let delta_q' = next succ_r'  in
      set_delta mgr q' delta_q';
      set_color mgr q' 0;

      (* Build the specular pair *)
      { ahw_regular = q; ahw_dual = q' }
    end

  let dual_weak_power_fusion mgr x r y =
    negate (weak_power_fusion mgr (negate x) r (negate y))

  let closure mgr nfa =
    if nfa = Nfa.nfa_false then empty mgr
    else begin
      let {Nfa.nfa_delta; nfa_start; nfa_final} = nfa in
      let n = Nfa.size nfa in

      (* Regular *)
      let state_map = Array.init n (fun i ->
        if nfa_delta.(i) != [] then new_state () else mgr.ahw_false) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_false then Label.dfalse
        else Label.dstate (state i)
      in

      (* Create the transitions and set the colors *)
      for i=0 to n-1 do
        if state i != mgr.ahw_false then begin
          let succ =
            List.fold_right (fun (b,j) s ->
              Label.dor (Label.dand b (lstate j)) s
            ) nfa_delta.(i) Label.dfalse
          in
          set_delta mgr (state i) (Label.simplify succ);
          set_color mgr (state i) 0
        end
      done;
      let ahw_regular = state nfa_start in

      (* Dual *)
      let state_map = Array.init n (fun i ->
        if nfa_delta.(i) != [] then new_state () else mgr.ahw_true) in

      let state i = state_map.(i) in
      let lstate i =
        let si = state i in
        if si = mgr.ahw_true then Label.dtrue
        else Label.dstate (state i)
      in

      (* Create the transitions and set the colors *)
      for i=0 to n-1 do
        if state i != mgr.ahw_true then begin
          let succ =
            List.fold_right (fun (b,j) s ->
              Label.dand (Label.dor (Label.dnot b) (lstate j)) s
            ) nfa_delta.(i) Label.dtrue
          in
          set_delta mgr (state i) (Label.simplify succ);
          set_color mgr (state i) 1
        end
      done;
      let ahw_dual = state nfa_start in

      disj mgr (concat mgr nfa (negate (empty mgr))) {ahw_regular;ahw_dual}
    end
end
