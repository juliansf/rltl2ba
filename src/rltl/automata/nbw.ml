open Shared

module type S = sig
  type error =
  | Invalid_Nbw

  exception Error of error

  module Ahw : Ahw.S

  type ranking = Shared.ranking

  type state = { s : int array; o : bool array; f : int array; ok : bool; }
  type trans = (state, Ahw.Nfa.Label.t) Hashtbl.t
  type reference = int

  type nbw =
    {
      nbw_delta : (state,trans) Hashtbl.t;
      nbw_init: (state,unit) Hashtbl.t;
    }

  type manager =
    { nbw_ahwmgr: Ahw.manager;
      nbw_aut_number: int ref;
      nbw_automata: (reference, nbw) Hashtbl.t;
    }

  type t = reference

  val _true : state

  val init: Ahw.manager -> manager

  val from_ahw: ?rank:ranking -> manager -> Ahw.t -> t
  val find: manager -> reference -> nbw
  val remove: manager -> reference -> unit

  val is_false: nbw -> bool
  val is_true: nbw -> bool

  (* Auxiliary functions *)
  val is_final: nbw -> state -> bool
  val get_delta: nbw -> state -> trans
end

module Make(A : Ahw.S) =
struct
  module Ahw = A
  module Label = Ahw.Nfa.Label
  module IS = Misc.IntSet
  module IntSetHashtbl = Misc.SetHashtbl(Misc.IntSet)

  type error =
  | Invalid_Nbw

  exception Error of error

  type ranking = Shared.ranking

  type state = { s : int array; o : bool array; f : int array; ok : bool; }
  type trans = (state, Ahw.Nfa.Label.t) Hashtbl.t
  type reference = int

  exception NodeFound of state
  exception StateFound of IS.t
  exception Node_found of int
  exception Node_not_found

  type nbw =
    {
      nbw_delta : (state,trans) Hashtbl.t;
      nbw_init: (state,unit) Hashtbl.t;
    }

  type manager =
    { nbw_ahwmgr: Ahw.manager;
      nbw_aut_number: int ref;
      nbw_automata: (reference, nbw) Hashtbl.t;
    }

  type t = reference

  let _true = {s=[||]; o=[||]; f=[||]; ok=true}

  (* Initialization function *)
  let init ahwmgr =
    {
      nbw_ahwmgr = ahwmgr;
      nbw_aut_number = ref (0);
      nbw_automata = Hashtbl.create 1;
    }

  let find mgr x = Hashtbl.find mgr.nbw_automata x
  let remove mgr x = Hashtbl.remove mgr.nbw_automata x

  let is_false aut = Hashtbl.length aut.nbw_delta = 0
  let is_true aut =
    Hashtbl.length aut.nbw_delta = 1
    && Hashtbl.mem aut.nbw_delta _true

  (* Auxiliary functions *)
  let new_reference mgr = incr mgr.nbw_aut_number; !(mgr.nbw_aut_number)
  let is_final aut q = q.ok = true
  let get_delta aut q = Hashtbl.find aut.nbw_delta q
  let set_delta aut q t = Hashtbl.replace aut.nbw_delta q t


  let rank mgr ahw rank_type max_val q =
    let ahw_size = Ahw.size mgr.nbw_ahwmgr ahw in
    let maxtwo_bad v =
      if v > 3 then List.map (( * ) 2) (Misc.range (v/2-1) (v/2))
      else if v >= 2 then [2]
      else [0]
    in
    match rank_type with
    | FullRank ->
      begin
        match Ahw.goodness mgr.nbw_ahwmgr q with
        | Ahw.Neutral -> Misc.range 1 max_val
        | Ahw.Bad -> List.map (( * ) 2) (Misc.range 1 (max_val / 2))
        | Ahw.Good -> [2*ahw_size]
      end
    | MaxTwoRank ->
      begin
        match Ahw.goodness mgr.nbw_ahwmgr q with
        | Ahw.Neutral -> if max_val > 1 then [max_val-1; max_val] else [1]
        | Ahw.Bad -> maxtwo_bad max_val
        | Ahw.Good -> [2*ahw_size]
      end
    | StratifiedRank ->
      begin
        let h = Ahw.get_stratum mgr.nbw_ahwmgr q in
        match Ahw.get_stratum_kind mgr.nbw_ahwmgr h with
        | Ahw.STransient -> [1]
        | Ahw.SAccept -> [1]
        | Ahw.SReject -> [2]
        | Ahw.SBuchi -> if Ahw.is_final mgr.nbw_ahwmgr q then [1] else [2]
        | Ahw.SCoBuchi ->
          let mv = min max_val (Ahw.get_stratum_size mgr.nbw_ahwmgr ahw h) in
          if Ahw.is_final mgr.nbw_ahwmgr q then maxtwo_bad mv
          else if mv > 1 then [mv-1; mv] else [1]
      end

  let combine_ranks ranks =
    let propagate x xss = List.map (fun xs -> x::xs) xss in
    let merge xs xss = List.concat (List.map (fun x -> propagate x xss) xs) in
    List.fold_right merge ranks [[]]

  let show_arrows x at =
    Format.eprintf "@[<v 2>{ ";
    IS.iter (fun i -> Format.eprintf "%d " i) x;
    Format.eprintf "}: %s %d:@;" "" (Hashtbl.hash at); (*(Label.to_string d);*)

    IntSetHashtbl.iter (fun is l ->
      Format.eprintf "{ ";
      IS.iter (fun i -> Format.eprintf "%d " i) is;
      Format.eprintf "} <- %s@;" (Label.to_string l);
    ) at;
    Format.eprintf "@]@."


  let printset fmt is =
    Format.fprintf fmt "[ %a]"
      (fun fmt -> IS.iter (fun i -> Format.fprintf fmt "%d " i)) is

  let show_arrows_dot init x fmt at =
    if x = init then
      Format.fprintf fmt
        ("init [shape=circle fillcolor=lightgray "
         ^^ "style=filled width=1.0 fontsize=20];@;")
    else
      Format.fprintf fmt
        "\"%a\" [shape=circle fillcolor=white style=filled];@;"
        printset x;

    let print_init_set fmt x =
      if x = init then  Format.fprintf fmt "init" else printset fmt x in

    IntSetHashtbl.iter (fun is l ->
      Format.fprintf fmt "\"%a\" -> \"%a\" [label=\"%s\"];@;"
        print_init_set x print_init_set is (Label.to_string l)
    ) at;
    Format.fprintf fmt "@;"

  let print_node fmt {s;o;f;ok} =
    Format.fprintf fmt "{ s=[ ";
    Array.iter (Format.fprintf fmt "%d ") s;
    Format.fprintf fmt "], o=[ ";
    Array.iteri (fun i b -> if b then Format.fprintf fmt "%d " (s.(i))) o;
    Format.fprintf fmt "], f=[ ";
    Array.iter (Format.fprintf fmt "%d ") f;
    Format.fprintf fmt "], ok=%B }" ok

  let show_delta_dot nmap istates node fmt dt =
    let isize = Hashtbl.length istates in
    let {s;ok} = node in
    let shapecolor = if ok then "doublecircle color=green" else "circle" in

    (*if Array.length s = 0 then
      Format.fprintf fmt
        ("T [shape=doublecircle color=green fillcolor=white "
         ^^ "style=filled width=0.7 fontsize=20];@;")
      else*) if Hashtbl.mem istates node then
      let idx =
        if isize > 1 then string_of_int (Hashtbl.find nmap node) else "" in
      Format.fprintf fmt
        ("init%s [shape=%s fillcolor=lightgray "
         ^^ "style=filled width=0.9 fontsize=20];@;")
      idx shapecolor
      else
      Format.fprintf fmt
        "\"%d\" [shape=%s fillcolor=white style=filled width=0.7 fontsize=15];@;"
        (Hashtbl.find nmap node) (*print_node node*) shapecolor;

    let print_init_node fmt x =
      if Hashtbl.mem istates x then
        let idx =
          if isize > 1 then string_of_int (Hashtbl.find nmap x) else "" in
        Format.fprintf fmt "init%s" idx
      else
        Format.fprintf fmt "%d" (Hashtbl.find nmap x) in

    Hashtbl.iter (fun node' l ->
      Format.fprintf fmt "\"%a\" -> \"%a\" [label=\" %s \" fontsize=16];@;"
        print_init_node node print_init_node node' (Label.to_string l)
    ) dt;
    Format.fprintf fmt "@;"


  let show_ranks rank_type mgr ahw =
    let size = Ahw.size mgr.nbw_ahwmgr ahw in
    let visited = Hashtbl.create size in
    let waiting = Queue.create () in
    Queue.add ahw waiting;
    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem visited x) then begin
        Hashtbl.add visited x ();
        Label.iter_states (fun i -> Queue.add i waiting)
          (Ahw.get_delta mgr.nbw_ahwmgr x);
        let r = rank mgr ahw rank_type (2*size) x in
        Printf.eprintf "*r:%d -> [ " x;
        List.iter (Printf.eprintf "%d ") r;
        Printf.eprintf "]\n";
      end
    done

  exception Found

  let equal_deltas (dx : trans) (dy : trans) =
      if Hashtbl.length dx = Hashtbl.length dy then begin
        try Hashtbl.iter (fun ix lx ->
            let ly = Hashtbl.find dy ix in
            if Label.compare_bool lx ly != 0 then raise Not_found
          ) dx; true
        with Not_found -> false
      end
      else false

  let equiv_node delta node_rel node_rel_map node =
    if Hashtbl.mem node_rel_map node then begin
      Hashtbl.find node_rel_map node
    end
    else begin
      let succ = Hashtbl.find delta node in
      let hash = Hashtbl.hash succ in
      if Hashtbl.mem node_rel hash then begin
        let candidates = Hashtbl.find node_rel hash in
        try Hashtbl.iter (fun succ' s ->
            if equal_deltas succ succ' && s.ok = node.ok then
              raise (NodeFound s)
          ) candidates;
          Hashtbl.add candidates succ node;
          Hashtbl.add node_rel_map node node;
          node
        with NodeFound s ->
          Hashtbl.add node_rel_map node s;
          s
      end
      else begin
        let t = Hashtbl.create 1 in
        Hashtbl.add t succ node;
        Hashtbl.add node_rel hash t;
        Hashtbl.add node_rel_map node node;
        node
      end
    end


  (****************************************************************************)
  (* Ranking Construction                                                     *)
  (****************************************************************************)
  let from_ahw_using_rankings ?rank:(rank_type=StratifiedRank) mgr ahw =
    let stratum = Ahw.get_stratum mgr.nbw_ahwmgr in
    let is_good x = Ahw.goodness mgr.nbw_ahwmgr x = Ahw.Good in
    let pred_set x =
      let pred = Ahw.pred mgr.nbw_ahwmgr x in
      match rank_type with
      | StratifiedRank ->
        IS.filter (fun y -> stratum y = stratum x) pred
      | _ -> pred
    in
    let rejecting_singleton h =
      match rank_type with
      | StratifiedRank -> (Ahw.get_stratum_size mgr.nbw_ahwmgr ahw h) = 1
      | _ -> false
    in

    let size = Ahw.size mgr.nbw_ahwmgr ahw in
    let init_disj = Label.disj_list (Ahw.get_init mgr.nbw_ahwmgr ahw) in
    let state_number = ref (-1) in
    let f_number = ref (-1) in
    let node_number = ref (-1) in

    let new_count r = incr r; !r in
    let new_state () = new_count state_number in
    let new_f () = new_count f_number in
    let new_node () = new_count node_number in

    let waiting = Queue.create () in
    let nbw_delta = IntSetHashtbl.create 8 in
    let cache = Hashtbl.create 8 in
(*  let initial = ref (IS.singleton ahw) in *)
    let initial = IntSetHashtbl.create (List.length init_disj) in
    let delta = Hashtbl.create 8 in
    let state_sets : int IntSetHashtbl.t = IntSetHashtbl.create 8 in
    let state_sets_reverse : (int, IS.t) Hashtbl.t = Hashtbl.create 8 in
    let f_funcs :
        ((state,int) Hashtbl.t, int) Hashtbl.t = Hashtbl.create 8 in
    let f_funcs_reverse :
        (int, (state,int) Hashtbl.t) Hashtbl.t = Hashtbl.create 8 in
    let initial_states = Hashtbl.create 8 in
    let nodes_map = Hashtbl.create 8 in
    let nodes_map_reverse = Hashtbl.create 8 in
    let node_rel  = Hashtbl.create 8 in
    let node_rel_map = Hashtbl.create 8 in
    let state_rel : (int, (Label.t IntSetHashtbl.t, IS.t) Hashtbl.t) Hashtbl.t =
      Hashtbl.create 8 in
    let state_rel_map : IS.t IntSetHashtbl.t = IntSetHashtbl.create 8 in
    let transient_states = IntSetHashtbl.create 8 in


    let equiv_node node = equiv_node delta node_rel node_rel_map node in

    let equiv_deltas (dx : Label.t IntSetHashtbl.t) (dy : Label.t IntSetHashtbl.t) =
      if IntSetHashtbl.length dx = IntSetHashtbl.length dy then begin
        try IntSetHashtbl.iter (fun ix lx ->
            let ly = IntSetHashtbl.find dy ix in
            if Label.compare_bool lx ly != 0 then raise Not_found
          ) dx; true
        with Not_found -> false
      end
      else false
    in

    let equiv_state (state : IS.t) =
      if IntSetHashtbl.mem state_rel_map state then begin
        IntSetHashtbl.find state_rel_map state
      end
      else begin
        let succ = IntSetHashtbl.find nbw_delta state in
        let hash = Hashtbl.hash succ in
        (*Format.eprintf "hash(succ(%a)) = %d@;" printset node hash;*)
        if Hashtbl.mem state_rel hash then begin
          let candidates : (Label.t IntSetHashtbl.t, IS.t) Hashtbl.t =
            Hashtbl.find state_rel hash in
          (*if Hashtbl.mem candidates succ then
            let s = Hashtbl.find candidates succ in
            IntSetHashtbl.add node_rel_map node s;
            s
          else begin*)
          try Hashtbl.iter (fun succ' s ->
              if equiv_deltas succ succ' then
                raise (StateFound s)
            ) candidates;
            Hashtbl.add candidates succ state;
            IntSetHashtbl.add state_rel_map state state;
            state
          with StateFound s ->
            IntSetHashtbl.add state_rel_map state s;
            s
        (*end*)
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ state;
          Hashtbl.add state_rel hash t;
          IntSetHashtbl.add state_rel_map state state;
          state
        end
      end
    in

    let replacing_state (state : IS.t) =
      if IntSetHashtbl.mem state_rel_map state then
        IntSetHashtbl.find state_rel_map state
      else
        state
    in

    let  compute () =
      let timer01 = new Misc.timer in
      timer01#start;
      List.iter (fun l ->
        let ls = List.fold_right IS.add (Label.states l) IS.empty in
        IntSetHashtbl.add initial ls ();
        Queue.add ls waiting;
      ) init_disj;
      (*Queue.add (!initial) waiting;*)
      while not (Queue.is_empty waiting) do
        let x = Queue.take waiting in
        if not (IntSetHashtbl.mem nbw_delta x) then begin
          Misc.IntSet.iter (fun i ->
            if not (Hashtbl.mem cache i) then
              let a = Label.arrows (Ahw.get_delta mgr.nbw_ahwmgr i) in
              Hashtbl.add cache i a
          ) x;
          let atrue = Label.arrows Label.dtrue in
          let a = IS.fold (fun i t ->
            (*************** This fold computation has a lot of room for improvement *)
            let ap = Label.arrows_product (Hashtbl.find cache i) t in
            ap
          ) x atrue in
          (*show_arrows x a;*)
          IntSetHashtbl.iter (fun y _ -> Queue.add y waiting) a;
          IntSetHashtbl.add nbw_delta x a;
          let i = new_state () in
          IntSetHashtbl.add state_sets x i;
          Hashtbl.add state_sets_reverse i x;
          (*let enode = equiv_node x in
            Format.eprintf "%a -> %a@\n@;@;" printset x printset enode;*)
        end;
      done;
      timer01#stop;
      Format.eprintf "Arrows product time: %f\n" (timer01#value);
      IntSetHashtbl.iter (fun x a ->
        Format.eprintf "node: "; (show_arrows x) a;
        ) nbw_delta;

      (* ... here the automaton can be simplified ... START *)
      let delta_size = IntSetHashtbl.length nbw_delta in
      let reverse = IntSetHashtbl.create delta_size in
      let dead = IntSetHashtbl.create 8 in
      let visited = IntSetHashtbl.create delta_size in
      let waiting = Queue.create () in

      (* Let's simplify it here by clashing equivalent nodes *)
      (* First, compute the SCC's *)
      (*Printf.eprintf "nbw_delta.size = %d\nstate_number+1 = %d\n"
      (IntSetHashtbl.length nbw_delta) (!state_number+1);*)
      let nbw_delta' = Array.init (!state_number+1) (fun i ->
          let a = IntSetHashtbl.find nbw_delta
              (Hashtbl.find state_sets_reverse i) in
          (*Printf.eprintf "a.size = %d\n" (IntSetHashtbl.length a);*)
          let succ = IntSetHashtbl.fold (fun is l xs ->
              (l, IntSetHashtbl.find state_sets is)::xs
            ) a [] in
          succ
        ) in
      let nbw_scc = Ahw.Nfa.tarjanSCC nbw_delta' in
      (*Array.iteri (fun sidx s ->
          Printf.eprintf "SCC(%d)[%d]: " sidx (Array.length s);
          Array.iter (fun i ->
              Printf.eprintf "%i " (i)
            ) s;
          Printf.eprintf "\n";
        ) nbw_scc;*)

      (* Second, compute the transient states *)
      Array.iter (fun states ->
          if Array.length states = 1 &&
             not (List.exists (fun (_,s) ->
                 s=states.(0)) nbw_delta'.(states.(0)))
          then
            IntSetHashtbl.add transient_states
              (Hashtbl.find state_sets_reverse (states.(0))) ();
        ) nbw_scc;

      (*Printf.eprintf "transient_states.size=%d\n"
        (IntSetHashtbl.length transient_states);
      IntSetHashtbl.iter (fun is _ ->
          Format.eprintf "%a@." printset is;
        ) transient_states;*)

      (* Third, compute the equivalent states *)
      (*Format.eprintf "nbw_delta.size = %d\n" (IntSetHashtbl.length nbw_delta);*)
      IntSetHashtbl.iter (fun x _ ->
          let x' = equiv_state x in ()
          (*Format.eprintf "%a --> %a@." printset x printset x';*)
          (*if x != x' then (* This state has an equivalent state. *)
            IntSetHashtbl.remove nbw_delta x;*)
        ) transient_states;

      (* Fourth, replace states by their equivalent states.*)
      begin
        try
          IntSetHashtbl.iter (fun x succ ->
              begin
                (* The state has no equivalent, we just need to modify its delta. *)
                let s = IntSetHashtbl.create (IntSetHashtbl.length succ) in
                IntSetHashtbl.iter (fun is l ->
                    let is = replacing_state is in
                    if IntSetHashtbl.mem s is then
                      let l' = IntSetHashtbl.find s is in
                      IntSetHashtbl.replace s is (Label.dor l l')
                    else
                      IntSetHashtbl.add s is l
                  ) succ;
                IntSetHashtbl.replace nbw_delta x s
              end
            ) nbw_delta;
        with Not_found -> begin
            Printf.eprintf "state_rel.size = %d\n" (Hashtbl.length state_rel);
            Hashtbl.iter (fun h _ ->
                Printf.eprintf "hash: %d\n" h;
              ) state_rel;
            failwith "Arg! not found...";
          end;
        end;
      Format.eprintf "nbw_delta.size = %d\n" (IntSetHashtbl.length nbw_delta);
      IntSetHashtbl.iter show_arrows nbw_delta;

      let get_reverse x =
        if not (IntSetHashtbl.mem reverse x) then
            IntSetHashtbl.add reverse x (IntSetHashtbl.create 2);
          IntSetHashtbl.find reverse x in

      IntSetHashtbl.iter (fun is _ -> Queue.add is waiting) initial;
      while not (Queue.is_empty waiting) do
        let y = Queue.take waiting in
        if not (IntSetHashtbl.mem visited y) then begin
          IntSetHashtbl.add visited y ();
          (*Format.eprintf "\n\nprocessing set: %a\n" printset y;*)

          let y_delta = IntSetHashtbl.find nbw_delta y in
          let y_delta_length = IntSetHashtbl.length y_delta in
          (*IntSetHashtbl.iter (fun x _ ->
            Format.eprintf "\ny_delta(%a) : %a\n" printset y printset x) y_delta;*)
          if y_delta_length = 0 then (* false state *)
            IntSetHashtbl.add dead y false
          else if IntSetHashtbl.mem y_delta IS.empty &&
              Label.is_true (IntSetHashtbl.find y_delta IS.empty) then
            IntSetHashtbl.add dead y true
          else
            IntSetHashtbl.iter (fun z _ ->
              if y != z && not (IS.is_empty z) then begin
                IntSetHashtbl.add (get_reverse z) y ();
                Queue.add z waiting
              end
            ) y_delta
        end
      done;
      (*IntSetHashtbl.iter (fun x _ -> Format.eprintf "dead: %a\n\n" printset x) dead;*)

      let boundary = IntSetHashtbl.create 8 in
      begin try
        while IntSetHashtbl.length dead > 0 do
          let t = IntSetHashtbl.copy dead in

          IntSetHashtbl.iter (fun x l ->
            IntSetHashtbl.remove initial x;
            if IntSetHashtbl.length initial = 0 then raise Exit;

            IntSetHashtbl.iter (fun y _ ->
              if y != x then IntSetHashtbl.add boundary y ()) (get_reverse x)
          ) t;
          IntSetHashtbl.reset dead;
          IntSetHashtbl.iter (fun x _ ->
            let x_delta = IntSetHashtbl.find nbw_delta x in begin
              try
                IntSetHashtbl.iter (fun y l ->
                  if IntSetHashtbl.mem t y then
                    if IntSetHashtbl.find t y then raise Exit
                    else IntSetHashtbl.remove x_delta y;
                ) x_delta
              with Exit -> (
                IntSetHashtbl.reset x_delta;
                IntSetHashtbl.add x_delta IS.empty Label.dtrue )
            end;

            if IntSetHashtbl.length x_delta = 0 then
              IntSetHashtbl.add dead x false
            else if IntSetHashtbl.mem x_delta IS.empty &&
                Label.is_true (IntSetHashtbl.find x_delta IS.empty) then
              IntSetHashtbl.add dead x true
          ) boundary;
          IntSetHashtbl.reset boundary
        done;
        with Exit -> (
          (*Format.eprintf "initial is dead: %a@." printset (!initial);*)
          (*IntSetHashtbl.remove nbw_delta (!initial);*)
          (*if IntSetHashtbl.find dead (!initial) then
            initial := IS.empty*)
        );
      end;
      (* ... here the automaton can be simplified ... END *)

      (*Format.eprintf "initial: %a@." printset (!initial);
      IntSetHashtbl.iter (fun x a ->
        Format.eprintf "simplified node: "; show_arrows x a
        ) nbw_delta;*)

      if IntSetHashtbl.length initial > 0 then begin
        let waiting = Queue.create () in
        IntSetHashtbl.iter (fun is _ ->
          let init_elements = IS.elements is in
          (*List.iter (Printf.eprintf "%d ") init_elements;*)
          (*Printf.eprintf "\n";*)
          let init_size = IS.cardinal is in
          let r_init =
            List.map (rank mgr ahw rank_type (2*size)) init_elements in
          let f_init = combine_ranks r_init in

          (* Create the initial states *)
          let s = Array.of_list init_elements in
          List.iter (fun fs ->
            let ok = ref true in
            let f = Array.of_list fs in
            let o = Array.init init_size (fun i ->
              let c = not (is_good (s.(i))) && f.(i) mod 2 = 0 in
              if c then ok := false; c) in
            if IntSetHashtbl.mem transient_states is then ok := false;
            let node = { s = s; o = o; f = f; ok = !ok; } in
            Hashtbl.add initial_states node ();
            Queue.add node waiting;
          ) f_init;
        ) initial;


        while not (Queue.is_empty waiting) do
          let node : state = Queue.take waiting in
          if not (Hashtbl.mem nodes_map node) then begin
            let node_idx = new_node () in
            Hashtbl.add nodes_map node (node_idx);
            Hashtbl.add nodes_map_reverse node_idx node;
            let {s;o;f;ok} = node in
            Format.eprintf "%d -> %a@." node_idx print_node node;

            let s_set = Array.fold_right IS.add s IS.empty in
            let arrows = Hashtbl.create 8 in
            let s_arrows = IntSetHashtbl.find nbw_delta s_set in
            (*show_arrows s_set s_arrows;*)
            let node_delta = Hashtbl.create (IntSetHashtbl.length s_arrows) in

            IntSetHashtbl.iter (fun is l ->
              let is_size = IS.cardinal is in
              let is_elements = IS.elements is in
              let s' = Array.of_list is_elements in
              let s'max = Array.map (fun q ->
                let pred = pred_set q in
                let pred_rank = Array.mapi (fun i x ->
                  if IS.mem x pred then f.(i) else 0) s in
                Array.fold_left (fun a b ->
                  if b=0 then a
                  else if a = 0 then b
                  else min a b) 0 pred_rank
              ) s' in
              let r_is = List.mapi (fun i q ->
                rank mgr ahw rank_type s'max.(i) q) is_elements in
              let f_is = combine_ranks r_is in
              List.iter (fun fs ->
                let ok' = ref true in
                let f' = Array.of_list fs in
                let o' = Array.init is_size (fun i ->
                  let c = not (is_good(s'.(i)))
                          && (*(rejecting_singleton (stratum (s'.(i))))
                               ||*) (if ok then f'.(i) mod 2 = 0
                          else begin
                            let pred = pred_set (s'.(i)) in
                            (*Printf.eprintf "%d -> " i;
                            IS.iter (Printf.eprintf "%d ") pred;
                              Printf.eprintf "\n";*)
                            try Array.iteri (fun j x ->
                              if IS.mem x pred && o.(j) && f'.(i) = f.(j) then
                                raise Exit) s;  false
                            with Exit -> true
                          end) in
                  if c then ok' := false; c) in
                if IntSetHashtbl.mem transient_states is then ok' := false;
                let node' = {s = s'; o = o'; f = f'; ok = !ok'} in
                Format.eprintf "    :: %a@." print_node node';
                Queue.add node' waiting;
                Hashtbl.add node_delta node' l;
              ) f_is;
            ) s_arrows;
            Hashtbl.add delta node node_delta;
            let enode = equiv_node node in ()
            (*Format.eprintf "%a -> %a\n" print_node node print_node enode;*)
            end;
        done;
      end;

      (* Printf.eprintf "f_init = [ "; *)
      (* List.iter (fun fs -> *)
      (*   Printf.eprintf "[ "; *)
      (*   List.iter (Printf.eprintf "%d ") fs; *)
      (*   Printf.eprintf "] "; *)
      (* ) f_init; *)
      (* Printf.eprintf "]\n"; *)
    in

    (*
    Printf.eprintf "Full:\n";
    show_ranks FullRank mgr ahw;
    Printf.eprintf "Max2:\n";
    show_ranks MaxTwoRank mgr ahw;
    Printf.eprintf "Stratified:\n";
    show_ranks StratifiedRank mgr ahw;
    *)

    let t_total,_ = Misc.chrono (fun _ ->
      if Ahw.bottom mgr.nbw_ahwmgr = ahw then ()
      else if Ahw.top mgr.nbw_ahwmgr = ahw then begin
        let true_state = {s=[||]; o=[||]; f=[||]; ok=true} in
        let true_delta = Hashtbl.create 1 in
        Hashtbl.add true_delta true_state Label.dtrue;
        Hashtbl.add delta true_state true_delta;
        Hashtbl.add initial_states true_state ()
      end
      else compute ()) () in

    (* Here the automaton can be simplified as well *)


    (* Clean the table of nodes - START *)
    let waiting = Queue.create () in
    let visited = Hashtbl.create (Hashtbl.length delta) in

    Hashtbl.iter (fun x _ ->
      Queue.add x waiting
    ) initial_states;

    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem visited x) then begin
        Hashtbl.add visited x ();
        (*Format.eprintf "%a@." print_node x;*)
        let x_delta = Hashtbl.find delta x in
        Hashtbl.iter (fun y _ -> Queue.add y waiting) x_delta
      end
    done;
    Hashtbl.iter (fun x _ ->
      if not (Hashtbl.mem visited x) then
        Hashtbl.remove delta x
    ) delta;
    (* Clean the table of nodes - END *)

    let _istates = Hashtbl.create (Hashtbl.length initial_states) in
    let _delta = Hashtbl.create (Hashtbl.length delta) in
    let _accept = Hashtbl.create 8 in
    let _reject = Hashtbl.create 8 in

    Hashtbl.iter (fun x succ ->
        let xid = Hashtbl.find nodes_map x in
        let succ' = Hashtbl.create (Hashtbl.length succ) in
        Hashtbl.iter (fun y l ->
            Hashtbl.add succ' (Hashtbl.find nodes_map y) l;
          ) succ;
        if x.ok then Hashtbl.add _accept xid ()
        else Hashtbl.add _reject xid ();
        if Hashtbl.mem initial_states x then Hashtbl.add _istates xid ();
        Hashtbl.add _delta xid succ';
      ) delta;

    let _icount = Hashtbl.length _istates in
    let _initial = if _icount > 1 then new_node ()
      else Hashtbl.fold (fun x _ _ -> x) _istates (-1) in

    if _icount > 1 then begin
      let ok = Hashtbl.fold (fun x _ b -> x.ok && b) initial_states true in
      if ok then Hashtbl.add _accept _initial () else Hashtbl.add _reject _initial ();
      Hashtbl.add nodes_map_reverse _initial {s=[|-1|]; o=[||]; f=[||]; ok=ok;};
      Hashtbl.add nodes_map {s=[|-1|]; o=[||]; f=[||]; ok=ok;} _initial;

      let _isucc = Hashtbl.create 8 in
      (try
         Hashtbl.iter (fun j _ ->
             let jsucc = Hashtbl.find _delta j in
             Hashtbl.iter (fun k l ->
                 if Hashtbl.mem _isucc k then
                   let l' = Hashtbl.find _isucc k in
                   Hashtbl.replace _isucc k (Label.dor l l')
                 else
                   Hashtbl.add _isucc k l
               ) jsucc;
           ) _istates;
         Hashtbl.add _delta _initial _isucc;
         Hashtbl.iter (fun i l ->
             Format.eprintf "%d <- %s@." i (Label.to_string l);
           ) _isucc;
       with _ -> failwith "nbw");

      Hashtbl.reset _istates;
      Hashtbl.add _istates _initial ();
    end;

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "%d[%s%s]: \n" i
          (if Hashtbl.mem _istates i then "i" else " ")
          (if Hashtbl.mem _accept i then "a" else " ");
        Hashtbl.iter (fun j l ->
            Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
          ) succ;
      ) _delta;

    let _delta_size = Hashtbl.length _delta in
    let _delta_map = Hashtbl.create _delta_size in
    let _node_rel_acc = Hashtbl.create _delta_size in
    let _node_rel_map_acc = Hashtbl.create _delta_size in
    let _node_rel_rej = Hashtbl.create _delta_size in
    let _node_rel_map_rej = Hashtbl.create _delta_size in
    (*let _node_rel_map_acc_rej = Hashtbl.create _delta_size in*)

    let is_accepting i = Hashtbl.mem _accept i in

    let _eq_deltas dx dy =
      if Hashtbl.length dx = Hashtbl.length dy then begin
        try Hashtbl.iter (fun ix lx ->
            let ly = Hashtbl.find dy ix in
            if Label.compare_bool lx ly != 0 then raise Not_found
          ) dx; true
        with Not_found -> false
      end
      else false
    in

    (* Compute equivalent rejecting states *)
    Hashtbl.iter (fun i _ ->
        let succ = Hashtbl.find _delta i in
        let hash = Hashtbl.hash succ in
        (*Printf.eprintf "node[%d][%d]\n" i hash;*)
        if Hashtbl.mem _node_rel_rej hash then begin
          let candidates = Hashtbl.find _node_rel_rej hash in
          try Hashtbl.iter (fun succ' j ->
              if _eq_deltas succ succ' then
                raise (Node_found j)
            ) candidates;
            Hashtbl.add candidates succ i;
            Hashtbl.add _node_rel_map_rej i i; (* This may be unnecessary *)
          with Node_found j ->
            Hashtbl.add _node_rel_map_rej i j
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ i;
          Hashtbl.add _node_rel_rej hash t;
          Hashtbl.add _node_rel_map_rej i i;
        end
      ) _reject;

    (*Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_rej(%d,%d)\n" i j
      ) _node_rel_map_rej;*)

    (* Compute equivalent accepting states *)
    Hashtbl.iter (fun i _ ->
        let succ = Hashtbl.find _delta i in
        let hash = Hashtbl.hash succ in
        (*Printf.eprintf "node[%d][%d]\n" i hash;*)
        if Hashtbl.mem _node_rel_acc hash then begin
          let candidates = Hashtbl.find _node_rel_acc hash in
          try Hashtbl.iter (fun succ' j ->
              if _eq_deltas succ succ' then
                raise (Node_found j)
            ) candidates;
            Hashtbl.add candidates succ i;
            Hashtbl.add _node_rel_map_acc i i; (* This may be unnecessary *)
          with Node_found j ->
            Hashtbl.add _node_rel_map_acc i j
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ i;
          Hashtbl.add _node_rel_acc hash t;
          Hashtbl.add _node_rel_map_acc i i;
        end
      ) _accept;


    Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_acc(%d,%d)\n" i j
      ) _node_rel_map_acc;

    Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_rej(%d,%d)\n" i j
      ) _node_rel_map_rej;

    let _replace_succ i succ =
      (* We assume that i has no equivalent node that can replace it. *)
      (*let succ = Hashtbl.find _delta i in*)
      Printf.eprintf "Replacing succ for %d\n" i;
      Printf.eprintf "%d[%s%s]: \n" i
        (if Hashtbl.mem _istates i then "i" else " ")
        (if Hashtbl.mem _accept i then "a" else " ");
      Hashtbl.iter (fun j l ->
          Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
        ) succ;

      let succ' = Hashtbl.create (Hashtbl.length succ) in
      Hashtbl.iter (fun k l ->
          Printf.eprintf "looking for %d..." k;
          let j =
            if is_accepting k then begin
              if k = i then i
              else Hashtbl.find _node_rel_map_acc k
            end
            else
              Hashtbl.find _node_rel_map_rej k
          in
          Printf.eprintf "j = %d\n" j;
          if Hashtbl.mem succ' j then
            let l' = Hashtbl.find succ' j in
            Hashtbl.replace succ' j (Label.dor l l')
          else
            Hashtbl.replace succ' j l;
          (*end*)
          Printf.eprintf "\n";
        ) succ;
      Hashtbl.replace _delta i succ';
      Printf.eprintf "Replacing succ for %d results in:\n" i;
      Printf.eprintf "%d[%s%s]: \n" i
        (if Hashtbl.mem _istates i then "i" else " ")
        (if Hashtbl.mem _accept i then "a" else " ");
      Hashtbl.iter (fun j l ->
          Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
        ) succ';
    in

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "processing %d... " i;
        if is_accepting i then begin
          Printf.eprintf "ACC... ";
          if i != Hashtbl.find _node_rel_map_acc i then begin
            Printf.eprintf "replaced by %d..."
              (Hashtbl.find _node_rel_map_acc i);
            Hashtbl.remove _delta i
          end
          else
            _replace_succ i succ
        end
        else if i != Hashtbl.find _node_rel_map_rej i then
          Hashtbl.remove _delta i
        else
          _replace_succ i succ;
        Printf.eprintf "\n";
      ) _delta;

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "%d[%s%s]: \n" i
          (if Hashtbl.mem _istates i then "i" else " ")
          (if Hashtbl.mem _accept i then "a" else " ");
        Hashtbl.iter (fun j l ->
            Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
          ) succ;
      ) _delta;


    let delta = Hashtbl.create (Hashtbl.length _delta) in
    Hashtbl.iter (fun i succ ->
        let succ' = Hashtbl.create (Hashtbl.length succ) in
        Hashtbl.iter (fun j l ->
            Hashtbl.add succ' (Hashtbl.find nodes_map_reverse j) l;
          ) succ;
        Hashtbl.add delta (Hashtbl.find nodes_map_reverse i) succ';
      ) _delta;

    let _initial =
      if is_accepting _initial then
        Hashtbl.find _node_rel_map_acc _initial
      else
        Hashtbl.find _node_rel_map_rej _initial
    in

    let initial_states = Hashtbl.create 1 in
      Hashtbl.add initial_states (Hashtbl.find nodes_map_reverse _initial) ();

    Format.eprintf "NBW creation time: %f@." t_total;
    Format.eprintf "States: %d@." (Hashtbl.length delta);

    Hashtbl.iter (fun x _ ->
        Format.eprintf "Initial state: %a@." print_node x;
      ) initial_states;
    let aut =
      {
        nbw_delta = delta;
        nbw_init = initial_states;
      }
    in
    let _ref = new_reference mgr in
    Hashtbl.add mgr.nbw_automata _ref aut;
    _ref


  (****************************************************************************)
  (* Generalized Construction                                                 *)
  (****************************************************************************)
  let gstate s = {s=s; o=[||]; f=[||]; ok=true}

  let from_ahw_using_generalized mgr ahw =
    from_ahw_using_rankings ~rank:StratifiedRank mgr ahw
(*
    let is_good x = Ahw.goodness mgr.nbw_ahwmgr x != Ahw.Good in
    let pred_set x = Ahw.pred mgr.nbw_ahwmgr x in

    let size = Ahw.size mgr.nbw_ahwmgr ahw in
    let init_disj = Label.disj_list (Ahw.get_init mgr.nbw_ahwmgr ahw) in
    let state_number = ref (-1) in
    let node_number = ref (-1) in

    let new_count r = incr r; !r in
    let new_state () = new_count state_number in
    let new_node () = new_count node_number in

    let waiting = Queue.create () in
    let nbw_delta = IntSetHashtbl.create 8 in
    let delta = Hashtbl.create 8 in
    let cache = Hashtbl.create 8 in
    let initial = IntSetHashtbl.create (List.length init_disj) in
    let initial_states = Hashtbl.create 8 in
    let state_sets : int IntSetHashtbl.t = IntSetHashtbl.create 8 in
    let state_sets_reverse : (int, IS.t) Hashtbl.t = Hashtbl.create 8 in
    let nodes_map = Hashtbl.create 8 in
    let nodes_map_reverse = Hashtbl.create 8 in
    let node_rel  = Hashtbl.create 8 in
    let node_rel_map = Hashtbl.create 8 in
    let state_rel : (int, (Label.t IntSetHashtbl.t, IS.t) Hashtbl.t) Hashtbl.t =
      Hashtbl.create 8 in
    let state_rel_map : IS.t IntSetHashtbl.t = IntSetHashtbl.create 8 in

    let equiv_node node = equiv_node delta node_rel node_rel_map node in

    let equiv_deltas (dx : Label.t IntSetHashtbl.t) (dy : Label.t IntSetHashtbl.t) =
      if IntSetHashtbl.length dx = IntSetHashtbl.length dy then begin
        try IntSetHashtbl.iter (fun ix lx ->
            let ly = IntSetHashtbl.find dy ix in
            if Label.compare_bool lx ly != 0 then raise Not_found
          ) dx; true
        with Not_found -> false
      end
      else false
    in


    let equiv_state (state : IS.t) =
      if IntSetHashtbl.mem state_rel_map state then begin
        IntSetHashtbl.find state_rel_map state
      end
      else begin
        let succ = IntSetHashtbl.find nbw_delta state in
        let hash = Hashtbl.hash succ in
        (*Format.eprintf "hash(succ(%a)) = %d@;" printset node hash;*)
        if Hashtbl.mem state_rel hash then begin
          let candidates : (Label.t IntSetHashtbl.t, IS.t) Hashtbl.t =
            Hashtbl.find state_rel hash in
          (*if Hashtbl.mem candidates succ then
            let s = Hashtbl.find candidates succ in
            IntSetHashtbl.add node_rel_map node s;
            s
          else begin*)
          try Hashtbl.iter (fun succ' s ->
              if equiv_deltas succ succ' then
                raise (StateFound s)
            ) candidates;
            Hashtbl.add candidates succ state;
            IntSetHashtbl.add state_rel_map state state;
            state
          with StateFound s ->
            IntSetHashtbl.add state_rel_map state s;
            s
        (*end*)
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ state;
          Hashtbl.add state_rel hash t;
          IntSetHashtbl.add state_rel_map state state;
          state
        end
      end
    in

    let replacing_state (state : IS.t) =
      if IntSetHashtbl.mem state_rel_map state then
        IntSetHashtbl.find state_rel_map state
      else
        state
    in

    let  compute () =
      let timer01 = new Misc.timer in
      timer01#start;

      (* Set up initial states *)
      List.iter (fun l ->
        let ls = List.fold_right IS.add (Label.states l) IS.empty in
        IntSetHashtbl.add initial ls ();
        Queue.add ls waiting;
      ) init_disj;

      while not (Queue.is_empty waiting) do
        let x = Queue.take waiting in
        if not (IntSetHashtbl.mem nbw_delta x) then begin
          Misc.IntSet.iter (fun i ->
            if not (Hashtbl.mem cache i) then
              let a = Label.arrows (Ahw.get_delta mgr.nbw_ahwmgr i) in
              Hashtbl.add cache i a
          ) x;
          let atrue = Label.arrows Label.dtrue in
          let a = IS.fold (fun i t ->
            (*************** This fold computation has a lot of room for improvement *)
            let ap = Label.arrows_product (Hashtbl.find cache i) t in
            ap
          ) x atrue in
          (*show_arrows x a;*)
          IntSetHashtbl.iter (fun y _ -> Queue.add y waiting) a;
          IntSetHashtbl.add nbw_delta x a;
          let i = new_state () in
          IntSetHashtbl.add state_sets x i;
          Hashtbl.add state_sets_reverse i x;
          (*let enode = equiv_node x in
            Format.eprintf "%a -> %a@\n@;@;" printset x printset enode;*)
        end;
      done;
      timer01#stop;
      Format.eprintf "Arrows product time: %f\n" (timer01#value);
      IntSetHashtbl.iter (fun x a ->
        Format.eprintf "node: "; (show_arrows x) a;
        ) nbw_delta;

      (* ... here the automaton can be simplified ... START *)
      let delta_size = IntSetHashtbl.length nbw_delta in
      let reverse = IntSetHashtbl.create delta_size in
      let dead = IntSetHashtbl.create 8 in
      let visited = IntSetHashtbl.create delta_size in
      let waiting = Queue.create () in

      (* Let's simplify it here by clashing equivalent nodes *)
      (* First, compute the SCC's *)
      (*Printf.eprintf "nbw_delta.size = %d\nstate_number+1 = %d\n"
      (IntSetHashtbl.length nbw_delta) (!state_number+1);*)
      let nbw_delta' = Array.init (!state_number+1) (fun i ->
          let a = IntSetHashtbl.find nbw_delta
              (Hashtbl.find state_sets_reverse i) in
          (*Printf.eprintf "a.size = %d\n" (IntSetHashtbl.length a);*)
          let succ = IntSetHashtbl.fold (fun is l xs ->
              (l, IntSetHashtbl.find state_sets is)::xs
            ) a [] in
          succ
        ) in
      let nbw_scc = Ahw.Nfa.tarjanSCC nbw_delta' in
      (*Array.iteri (fun sidx s ->
          Printf.eprintf "SCC(%d)[%d]: " sidx (Array.length s);
          Array.iter (fun i ->
              Printf.eprintf "%i " (i)
            ) s;
          Printf.eprintf "\n";
        ) nbw_scc;*)

      (* Second, compute the transient states *)

      (* Third, compute the equivalent states *)
      (*Format.eprintf "nbw_delta.size = %d\n" (IntSetHashtbl.length nbw_delta);*)
      IntSetHashtbl.iter (fun x _ ->
          let x' = equiv_state x in ()
          (*Format.eprintf "%a --> %a@." printset x printset x';*)
          (*if x != x' then (* This state has an equivalent state. *)
            IntSetHashtbl.remove nbw_delta x;*)
        ) transient_states;

      (* Fourth, replace states by their equivalent states.*)
      begin
        try
          IntSetHashtbl.iter (fun x succ ->
              begin
                (* The state has no equivalent, we just need to modify its delta. *)
                let s = IntSetHashtbl.create (IntSetHashtbl.length succ) in
                IntSetHashtbl.iter (fun is l ->
                    let is = replacing_state is in
                    if IntSetHashtbl.mem s is then
                      let l' = IntSetHashtbl.find s is in
                      IntSetHashtbl.replace s is (Label.dor l l')
                    else
                      IntSetHashtbl.add s is l
                  ) succ;
                IntSetHashtbl.replace nbw_delta x s
              end
            ) nbw_delta;
        with Not_found -> begin
            Printf.eprintf "state_rel.size = %d\n" (Hashtbl.length state_rel);
            Hashtbl.iter (fun h _ ->
                Printf.eprintf "hash: %d\n" h;
              ) state_rel;
            failwith "Arg! not found...";
          end;
        end;
      Format.eprintf "nbw_delta.size = %d\n" (IntSetHashtbl.length nbw_delta);
      IntSetHashtbl.iter show_arrows nbw_delta;

      let get_reverse x =
        if not (IntSetHashtbl.mem reverse x) then
            IntSetHashtbl.add reverse x (IntSetHashtbl.create 2);
          IntSetHashtbl.find reverse x in

      IntSetHashtbl.iter (fun is _ -> Queue.add is waiting) initial;
      while not (Queue.is_empty waiting) do
        let y = Queue.take waiting in
        if not (IntSetHashtbl.mem visited y) then begin
          IntSetHashtbl.add visited y ();
          (*Format.eprintf "\n\nprocessing set: %a\n" printset y;*)

          let y_delta = IntSetHashtbl.find nbw_delta y in
          let y_delta_length = IntSetHashtbl.length y_delta in
          (*IntSetHashtbl.iter (fun x _ ->
            Format.eprintf "\ny_delta(%a) : %a\n" printset y printset x) y_delta;*)
          if y_delta_length = 0 then (* false state *)
            IntSetHashtbl.add dead y false
          else if IntSetHashtbl.mem y_delta IS.empty &&
              Label.is_true (IntSetHashtbl.find y_delta IS.empty) then
            IntSetHashtbl.add dead y true
          else
            IntSetHashtbl.iter (fun z _ ->
              if y != z && not (IS.is_empty z) then begin
                IntSetHashtbl.add (get_reverse z) y ();
                Queue.add z waiting
              end
            ) y_delta
        end
      done;
      (*IntSetHashtbl.iter (fun x _ -> Format.eprintf "dead: %a\n\n" printset x) dead;*)

      let boundary = IntSetHashtbl.create 8 in
      begin try
        while IntSetHashtbl.length dead > 0 do
          let t = IntSetHashtbl.copy dead in

          IntSetHashtbl.iter (fun x l ->
            IntSetHashtbl.remove initial x;
            if IntSetHashtbl.length initial = 0 then raise Exit;

            IntSetHashtbl.iter (fun y _ ->
              if y != x then IntSetHashtbl.add boundary y ()) (get_reverse x)
          ) t;
          IntSetHashtbl.reset dead;
          IntSetHashtbl.iter (fun x _ ->
            let x_delta = IntSetHashtbl.find nbw_delta x in begin
              try
                IntSetHashtbl.iter (fun y l ->
                  if IntSetHashtbl.mem t y then
                    if IntSetHashtbl.find t y then raise Exit
                    else IntSetHashtbl.remove x_delta y;
                ) x_delta
              with Exit -> (
                IntSetHashtbl.reset x_delta;
                IntSetHashtbl.add x_delta IS.empty Label.dtrue )
            end;

            if IntSetHashtbl.length x_delta = 0 then
              IntSetHashtbl.add dead x false
            else if IntSetHashtbl.mem x_delta IS.empty &&
                Label.is_true (IntSetHashtbl.find x_delta IS.empty) then
              IntSetHashtbl.add dead x true
          ) boundary;
          IntSetHashtbl.reset boundary
        done;
        with Exit -> (
          (*Format.eprintf "initial is dead: %a@." printset (!initial);*)
          (*IntSetHashtbl.remove nbw_delta (!initial);*)
          (*if IntSetHashtbl.find dead (!initial) then
            initial := IS.empty*)
        );
      end;
      (* ... here the automaton can be simplified ... END *)

      Format.eprintf "initial: %a@." printset (!initial);
      IntSetHashtbl.iter (fun x a ->
        Format.eprintf "simplified node: "; show_arrows x a
        ) nbw_delta;
    in


    let t_total,_ = Misc.chrono (fun _ ->
      (*if Ahw.bottom mgr.nbw_ahwmgr = ahw then ()
      else if Ahw.top mgr.nbw_ahwmgr = ahw then begin
        let true_state = {s=[||]; o=[||]; f=[||]; ok=true} in
        let true_delta = Hashtbl.create 1 in
        Hashtbl.add true_delta true_state Label.dtrue;
        Hashtbl.add delta true_state true_delta;
        Hashtbl.add initial_states true_state ()
      end
        else *)compute ()) () in

    (* Here the automaton can be simplified as well *)


    (* Clean the table of nodes - START *)
    let waiting = Queue.create () in
    let visited = Hashtbl.create (Hashtbl.length delta) in

    Hashtbl.iter (fun x _ ->
      Queue.add x waiting
    ) initial_states;

    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem visited x) then begin
        Hashtbl.add visited x ();
        (*Format.eprintf "%a@." print_node x;*)
        let x_delta = Hashtbl.find delta x in
        Hashtbl.iter (fun y _ -> Queue.add y waiting) x_delta
      end
    done;
    Hashtbl.iter (fun x _ ->
      if not (Hashtbl.mem visited x) then
        Hashtbl.remove delta x
    ) delta;
    (* Clean the table of nodes - END *)

    let _istates = Hashtbl.create (Hashtbl.length initial_states) in
    let _delta = Hashtbl.create (Hashtbl.length delta) in
    let _accept = Hashtbl.create 8 in
    let _reject = Hashtbl.create 8 in

    Hashtbl.iter (fun x succ ->
        let xid = Hashtbl.find nodes_map x in
        let succ' = Hashtbl.create (Hashtbl.length succ) in
        Hashtbl.iter (fun y l ->
            Hashtbl.add succ' (Hashtbl.find nodes_map y) l;
          ) succ;
        if x.ok then Hashtbl.add _accept xid ()
        else Hashtbl.add _reject xid ();
        if Hashtbl.mem initial_states x then Hashtbl.add _istates xid ();
        Hashtbl.add _delta xid succ';
      ) delta;

    let _icount = Hashtbl.length _istates in
    let _initial = if _icount > 1 then new_node ()
      else Hashtbl.fold (fun x _ _ -> x) _istates (-1) in

    if _icount > 1 then begin
      let ok = Hashtbl.fold (fun x _ b -> x.ok && b) initial_states true in
      if ok then Hashtbl.add _accept _initial () else Hashtbl.add _reject _initial ();
      Hashtbl.add nodes_map_reverse _initial {s=[|-1|]; o=[||]; f=[||]; ok=ok;};
      Hashtbl.add nodes_map {s=[|-1|]; o=[||]; f=[||]; ok=ok;} _initial;

      let _isucc = Hashtbl.create 8 in
      (try
         Hashtbl.iter (fun j _ ->
             let jsucc = Hashtbl.find _delta j in
             Hashtbl.iter (fun k l ->
                 if Hashtbl.mem _isucc k then
                   let l' = Hashtbl.find _isucc k in
                   Hashtbl.replace _isucc k (Label.dor l l')
                 else
                   Hashtbl.add _isucc k l
               ) jsucc;
           ) _istates;
         Hashtbl.add _delta _initial _isucc;
         Hashtbl.iter (fun i l ->
             Format.eprintf "%d <- %s@." i (Label.to_string l);
           ) _isucc;
       with _ -> failwith "nbw");

      Hashtbl.reset _istates;
      Hashtbl.add _istates _initial ();
    end;

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "%d[%s%s]: \n" i
          (if Hashtbl.mem _istates i then "i" else " ")
          (if Hashtbl.mem _accept i then "a" else " ");
        Hashtbl.iter (fun j l ->
            Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
          ) succ;
      ) _delta;

    let _delta_size = Hashtbl.length _delta in
    let _delta_map = Hashtbl.create _delta_size in
    let _node_rel_acc = Hashtbl.create _delta_size in
    let _node_rel_map_acc = Hashtbl.create _delta_size in
    let _node_rel_rej = Hashtbl.create _delta_size in
    let _node_rel_map_rej = Hashtbl.create _delta_size in

    let is_accepting i = Hashtbl.mem _accept i in

    let _eq_deltas dx dy =
      if Hashtbl.length dx = Hashtbl.length dy then begin
        try Hashtbl.iter (fun ix lx ->
            let ly = Hashtbl.find dy ix in
            if Label.compare_bool lx ly != 0 then raise Not_found
          ) dx; true
        with Not_found -> false
      end
      else false
    in

    (* Compute equivalent rejecting states *)
    Hashtbl.iter (fun i _ ->
        let succ = Hashtbl.find _delta i in
        let hash = Hashtbl.hash succ in
        (*Printf.eprintf "node[%d][%d]\n" i hash;*)
        if Hashtbl.mem _node_rel_rej hash then begin
          let candidates = Hashtbl.find _node_rel_rej hash in
          try Hashtbl.iter (fun succ' j ->
              if _eq_deltas succ succ' then
                raise (Node_found j)
            ) candidates;
            Hashtbl.add candidates succ i;
            Hashtbl.add _node_rel_map_rej i i; (* This may be unnecessary *)
          with Node_found j ->
            Hashtbl.add _node_rel_map_rej i j
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ i;
          Hashtbl.add _node_rel_rej hash t;
          Hashtbl.add _node_rel_map_rej i i;
        end
      ) _reject;

    (*Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_rej(%d,%d)\n" i j
      ) _node_rel_map_rej;*)

    (* Compute equivalent accepting states *)
    Hashtbl.iter (fun i _ ->
        let succ = Hashtbl.find _delta i in
        let hash = Hashtbl.hash succ in
        (*Printf.eprintf "node[%d][%d]\n" i hash;*)
        if Hashtbl.mem _node_rel_acc hash then begin
          let candidates = Hashtbl.find _node_rel_acc hash in
          try Hashtbl.iter (fun succ' j ->
              if _eq_deltas succ succ' then
                raise (Node_found j)
            ) candidates;
            Hashtbl.add candidates succ i;
            Hashtbl.add _node_rel_map_acc i i; (* This may be unnecessary *)
          with Node_found j ->
            Hashtbl.add _node_rel_map_acc i j
        end
        else begin
          let t = Hashtbl.create 1 in
          Hashtbl.add t succ i;
          Hashtbl.add _node_rel_acc hash t;
          Hashtbl.add _node_rel_map_acc i i;
        end
      ) _accept;


    Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_acc(%d,%d)\n" i j
      ) _node_rel_map_acc;

    Hashtbl.iter (fun i j ->
        Printf.eprintf "equivalent_rej(%d,%d)\n" i j
      ) _node_rel_map_rej;

    let _replace_succ i succ =
      (* We assume that i has no equivalent node that can replace it. *)
      (*let succ = Hashtbl.find _delta i in*)
      Printf.eprintf "Replacing succ for %d\n" i;
      Printf.eprintf "%d[%s%s]: \n" i
        (if Hashtbl.mem _istates i then "i" else " ")
        (if Hashtbl.mem _accept i then "a" else " ");
      Hashtbl.iter (fun j l ->
          Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
        ) succ;

      let succ' = Hashtbl.create (Hashtbl.length succ) in
      Hashtbl.iter (fun k l ->
          Printf.eprintf "looking for %d..." k;
          let j =
            if is_accepting k then begin
              if k = i then i
              else Hashtbl.find _node_rel_map_acc k
            end
            else
              Hashtbl.find _node_rel_map_rej k
          in
          Printf.eprintf "j = %d\n" j;
          if Hashtbl.mem succ' j then
            let l' = Hashtbl.find succ' j in
            Hashtbl.replace succ' j (Label.dor l l')
          else
            Hashtbl.replace succ' j l;
          (*end*)
          Printf.eprintf "\n";
        ) succ;
      Hashtbl.replace _delta i succ';
      Printf.eprintf "Replacing succ for %d results in:\n" i;
      Printf.eprintf "%d[%s%s]: \n" i
        (if Hashtbl.mem _istates i then "i" else " ")
        (if Hashtbl.mem _accept i then "a" else " ");
      Hashtbl.iter (fun j l ->
          Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
        ) succ';
    in

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "processing %d... " i;
        if is_accepting i then begin
          Printf.eprintf "ACC... ";
          if i != Hashtbl.find _node_rel_map_acc i then begin
            Printf.eprintf "replaced by %d..."
              (Hashtbl.find _node_rel_map_acc i);
            Hashtbl.remove _delta i
          end
          else
            _replace_succ i succ
        end
        else if i != Hashtbl.find _node_rel_map_rej i then
          Hashtbl.remove _delta i
        else
          _replace_succ i succ;
        Printf.eprintf "\n";
      ) _delta;

    Hashtbl.iter (fun i succ ->
        Printf.eprintf "%d[%s%s]: \n" i
          (if Hashtbl.mem _istates i then "i" else " ")
          (if Hashtbl.mem _accept i then "a" else " ");
        Hashtbl.iter (fun j l ->
            Printf.eprintf "    %d <- %s\n" j (Label.to_string l);
          ) succ;
      ) _delta;


    let delta = Hashtbl.create (Hashtbl.length _delta) in
    Hashtbl.iter (fun i succ ->
        let succ' = Hashtbl.create (Hashtbl.length succ) in
        Hashtbl.iter (fun j l ->
            Hashtbl.add succ' (Hashtbl.find nodes_map_reverse j) l;
          ) succ;
        Hashtbl.add delta (Hashtbl.find nodes_map_reverse i) succ';
      ) _delta;

    let _initial =
      if is_accepting _initial then
        Hashtbl.find _node_rel_map_acc _initial
      else
        Hashtbl.find _node_rel_map_rej _initial
    in

    let initial_states = Hashtbl.create 1 in
      Hashtbl.add initial_states (Hashtbl.find nodes_map_reverse _initial) ();

    Format.eprintf "NBW creation time: %f@." t_total;
    Format.eprintf "States: %d@." (Hashtbl.length delta);

    Hashtbl.iter (fun x _ ->
        Format.eprintf "Initial state: %a@." print_node x;
      ) initial_states;
    let aut =
      {
        nbw_delta = delta;
        nbw_init = initial_states;
      }
    in
    let _ref = new_reference mgr in
    Hashtbl.add mgr.nbw_automata _ref aut;
    _ref*)

  (****************************************************************************)
  (* Construction selector                                                    *)
  (****************************************************************************)
  let from_ahw ?rank:(rank_type=StratifiedRank) mgr ahw =
    if Ahw.is_very_weak mgr.nbw_ahwmgr ahw then begin
      Printf.eprintf "AHW is VERY WEAK!\n";
      Printf.eprintf "Applying generalized construction.\n";
      from_ahw_using_generalized mgr ahw
    end
    else begin
      Printf.eprintf "AHW is NOT VERY WEAK!\n";
      Printf.eprintf "Applying ranking construction.\n";
      from_ahw_using_rankings ~rank:rank_type mgr ahw
    end
end
