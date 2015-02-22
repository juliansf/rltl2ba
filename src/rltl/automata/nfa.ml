module type S = sig
  type error =
  | Invalid_Nfa

  exception Error of error

  module Label : Bool.S

  type label = Label.t
  type trans = (label * int) list
  type t =
    { nfa_delta: trans array;
      nfa_start: int;
      nfa_final: bool array;
    }

  val size : t -> int

  val nfa_false : t
  val letter : label -> t
  val star : t -> t
  val concat : t -> t -> t
  val fusion : t -> t -> t
  val plus : t -> t -> t
  val product : t -> t -> t
end

module Make(B : Bool.S) =
struct
  let trigger = ref false



  type error =
  | Invalid_Nfa

  exception Error of error

  module Label = B

  type label = Label.t
  type trans = (Label.t * int) list
  type t =
    { nfa_delta: trans array;
      nfa_start: int;
      nfa_final: bool array;
    }

  let _false = { nfa_delta = [||]; nfa_start = -1; nfa_final = [||]; }
  let nfa_false = _false

(** Number of states of an NFA *)
  let size nfa = Array.length nfa.nfa_delta


(** Shifts the state numbering of a transition by a constant *)
  let shift_k k (delta : trans array) =
    Array.map (List.map (fun (g,i) -> (g,i+k))) delta


(** Reverse the transition relation *)
  let reverse_delta delta =
    let n = Array.length delta in
    let rev = Array.make n [] in
    Array.iteri (fun i succ ->
      List.iter (fun (b,q) -> rev.(q) <- (b,i)::rev.(q)) succ
    ) delta;
    rev


(** Mark which states are reachable *)
  let mark_reachable (start: int list) delta =
    let reachable = Array.make (Array.length delta) false in
    let waiting = Queue.create () in

  (* Mark the starting states as reachable *)
    List.iter (fun i ->
      reachable.(i) <- true;
      Queue.add i waiting
    ) start;

  (* Mark reachable states *)
    while not (Queue.is_empty waiting) do
      let q = Queue.take waiting in
      let f waiting (_,q') =
      (* Check if q' is already reachable *)
        if not reachable.(q') then begin
          Queue.add q' waiting;
          reachable.(q') <- true
        end
      in
      List.iter (f waiting) delta.(q)
    done;
    reachable

  let mark_forward_reachable nfa =
    mark_reachable [nfa.nfa_start] nfa.nfa_delta

  let mark_backward_reachable nfa =
    let final,_ =
      Array.fold_left (fun (lst,n) b ->
        (if b then n::lst else lst), n+1
      ) ([],0) nfa.nfa_final
    in
    let rev_delta = reverse_delta nfa.nfa_delta in
    mark_reachable final rev_delta


  (** Remove unreachable states *)
  let remove_unreachable nfa =
    let n = size nfa in
    let reachable = mark_forward_reachable nfa in
    let bwd_reachable = mark_backward_reachable nfa in
    for i=0 to n-1 do
      reachable.(i) <- reachable.(i) && bwd_reachable.(i)
    done;

    (* Compute the new mapping *)
    let mapping = Array.make n (-1) in
    let (_,n') =
      Array.fold_left (fun (i,k) r ->
        if r then (mapping.(i) <- k; (i+1,k+1))
        else (i+1,k)
      ) (0,0) reachable
    in

    (* Build the new transtition function and the acceptance condition *)
    let delta = Array.make n' [] in
    let final = Array.make n' false in
    let map_st i i' =
      if i' >= 0 then begin
        delta.(i') <-
          List.fold_left (fun succ (b,q) ->
            if mapping.(q) < 0 then succ else (b,mapping.(q))::succ
          ) [] nfa.nfa_delta.(i);
        final.(i') <- nfa.nfa_final.(i)
      end
    in
    Array.iteri map_st mapping;

    (* Build the automata *)
    {
      nfa_delta = delta;
      nfa_start = mapping.(nfa.nfa_start);
      nfa_final = final;
    }


  (** Normalize the transition of a state *)
  let normalize succ =
    let rec norm lst =
      match lst with
      | [] -> []
      | (l1,q1)::(l2,q2)::lst when q1=q2 ->
        norm ((Label.simplify (Label.dor l1 l2), q1)::lst)
      | (l,_)::lst when Label.is_false l -> lst
      | (b,q)::lst -> (Label.simplify b, q) :: norm lst
    in
    norm (List.fast_sort (fun (_,q1) (_,q2) -> compare q1 q2) succ)


  (** Merge states with same successors that are both final/non-final *)
  let merge_states nfa =
    let module Map = Hashtbl in
    let size = size nfa in

    (* Normalize the successors *)
    for i=0 to size-1 do
      nfa.nfa_delta.(i) <- normalize nfa.nfa_delta.(i)
    done;

    (* Merge final/non-final states *)
    let merge remaps nfa final_flag =
      let {nfa_delta=delta; nfa_start=start; nfa_final=final} = nfa in

      (* succ list -> state *)
      let succ_map = Map.create size in

      (* state -> state *)
      let state_map = Map.create size in

      (* Redirect states based on its successors *)
      let redirect i succ =
        if final.(i)=final_flag then begin
          if not (Map.mem succ_map succ) then
            Map.add succ_map succ i
          else
            Map.add state_map i (Map.find succ_map succ)
        end
      in
      redirect start delta.(start);
      Array.iteri redirect delta;

      (* Remap successor states *)
      let rename succ =
        let cnt = !remaps in
        let succ =
          List.map (fun (b,i) ->
            if Map.mem state_map i then
              let j = Map.find state_map i in
              (if i!=j then incr remaps; (b,j))
            else (b,i))
            succ
        in
        succ (*if cnt = !remaps then succ else normalize succ*)
      in
      let delta = Array.map rename delta in
      {
        nfa_delta = delta;
        nfa_start = start;
        nfa_final = final;
      }
    in

    let nfa = merge (ref 0) nfa true in

    let remaps = ref 0 in
    let nfa' = ref (merge remaps nfa false) in
    while !remaps > 0 do
      remaps := 0;
      nfa' := merge remaps !nfa' false;
    done;
    !nfa'

  let is_well_formed nfa =
    let delta_size = Array.length nfa.nfa_delta in
    let final_size = Array.length nfa.nfa_final in
    delta_size = final_size &&
        nfa.nfa_start >= 0 && nfa.nfa_start < delta_size


  let count = ref 0
  let simplify nfa =
    incr count;
    (*Printf.fprintf stderr "Simplifying(%d)... %!" !count;*)
    let x =
      if nfa = _false then _false
      else if is_well_formed nfa
      then remove_unreachable (merge_states nfa)
      else raise (Error Invalid_Nfa)
    in
    (*Printf.fprintf stderr "done\n%!";*)
    x


  (** Builds the NFA of a letter (Label.t) *)
  let letter b =
    if Label.is_false b then _false
    else
      {
        nfa_delta = [|[(b,1)];[]|];
        nfa_start = 0;
        nfa_final = [|false; true|];
      }

  (** Builds the Kleen star NFA of an NFA *)
  let star nfa =
    if nfa=_false then _false else begin
      let {nfa_delta=delta; nfa_start=start; nfa_final=final} = nfa in
      let n = size nfa in

      let delta' = Array.make (n+1) [] in
      let final' = Array.append final [|true|] in

    (* Save the successor list of the inital state *)
      let start_succ = delta.(start) in

    (* from final states also to the successors of the initial state *)
      Array.iteri (fun i succ ->
        delta'.(i) <-
          if final.(i)
          then List.append succ start_succ
          else succ
      ) delta;

      delta'.(n) <- start_succ;
      simplify { nfa_delta=delta'; nfa_start=n; nfa_final=final'; }
    end


  (** Concatenates two NFAs *)
  let concat nfa1 nfa2 =
    if nfa1=_false || nfa2=_false then _false else begin
      let {nfa_delta=delta1; nfa_start=start1; nfa_final=final1} = nfa1 in
      let {nfa_delta=delta2; nfa_start=start2; nfa_final=final2} = nfa2 in

      let n1,n2 = size nfa1, size nfa2 in
      let delta = Array.make (n1+n2) [] in

    (* Shift transition of [nfa2] by [n1] *)
      let delta2 = shift_k n1 delta2 in

    (* Add out edges of the start node of [nfa2] to finals
       states of [nfa1] *)
      Array.iteri (fun i succ ->
        delta.(i) <- succ @ if final1.(i) then delta2.(start2) else []
      ) delta1;

      Array.iteri (fun i succ -> delta.(n1+i) <- succ) delta2;

      let final =
        let final1 =
          if final2.(start2) then final1 else Array.make n1 false in
        Array.append final1 final2
      in
      simplify { nfa_delta=delta; nfa_start=start1; nfa_final=final; }
    end


(** Concatenates two NFAs with an overlapping letter *)
  let fusion nfa1 nfa2 =
    if nfa1=_false || nfa2=_false then _false else begin
      let {nfa_delta=delta1; nfa_start=start1; nfa_final=final1} = nfa1 in
      let {nfa_delta=delta2; nfa_start=start2; nfa_final=final2} = nfa2 in

      let n1 = size nfa1 in

    (* Shift transition of [nfa2] by [n1] *)
      let delta2 = shift_k n1 delta2 in

      let delta = Array.append delta1 delta2 in

    (* Combine edges to final states of [nfa1] with edges of the start
       node of [nfa2] *)
      Array.iteri (fun i succ ->
        List.iter (fun (b,i') ->
          if final1.(i') then
            List.iter (fun (c,k) ->
              delta.(i) <- (Label.dand b c,k)::succ) delta2.(start2)
        ) succ
      ) delta1;

      let final = Array.append (Array.make n1 false) final2 in

      simplify { nfa_delta=delta; nfa_start=start1; nfa_final=final; }
    end


(** Builds the union of two NFAs *)
  let plus nfa1 nfa2 =
    if nfa1=_false then nfa2
    else if nfa2=_false then nfa1 else begin
      let {nfa_delta=delta1; nfa_start=start1; nfa_final=final1} = nfa1 in
      let {nfa_delta=delta2; nfa_start=start2; nfa_final=final2} = nfa2 in

      let n1,n2 = size nfa1, size nfa2 in
      let delta2 = shift_k n1 delta2 in
      let delta =
        Array.append delta1 (
          Array.append delta2 [|delta1.(start1) @ delta2.(start2)|]) in
      let final =
        Array.append final1 (
          Array.append final2 [| final1.(start1) || final2.(start2) |]) in

      simplify
        {
          nfa_delta=delta;
          nfa_start=n1+n2;
          nfa_final=final;
        }
    end


(** Builds the product of two NFAs *)
  let product nfa1 nfa2 =
    if nfa1=_false || nfa2=_false then _false else begin
      let {nfa_delta=delta1; nfa_start=start1; nfa_final=final1} = nfa1 in
      let {nfa_delta=delta2; nfa_start=start2; nfa_final=final2} = nfa2 in

      let n1,n2 = size nfa1, size nfa2 in

      let delta = Array.make (n1*n2) [] in
      let start = start1*n2+start2 in
      let final = Array.make (n1*n2) false in

      for i=0 to n1-1 do
        for j=0 to n2-1 do
          let k = i*n2 + j in
          final.(k) <- final1.(i) && final2.(j);
          List.iter (fun (b,i') ->
            List.iter (fun (c,j') ->
              delta.(k) <- (Label.dand b c, i'*n2+j')::delta.(k)
            ) delta2.(j)) delta1.(i)
        done;
      done;
      simplify {
        nfa_delta=delta;
        nfa_start=start;
        nfa_final=final;
      }
    end
end
