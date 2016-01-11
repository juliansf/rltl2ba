module Ahw = Ahw.Make(Nfa.Make(Bool.Default.B))
open Ahw

(*let color = [|"gray"; "red"; "green"|]*)

let stratum_kind mgr h =
  match get_stratum_kind mgr h with
  | SAccept -> "A"
  | SReject -> "R"
  | SBuchi -> "B"
  | SCoBuchi -> "C"
  | STransient -> "T"

let stratum_color mgr h =
  match get_stratum_kind mgr h with
  | SAccept -> "#94E9BF"
  | SReject -> "#FF8383"
  | SBuchi -> "#99CCFF"
  | SCoBuchi -> "#DADADA"
  | STransient -> "#D494D4"


let state_names : (state,state) Hashtbl.t = Hashtbl.create 8
let state_count = ref (-1)
let strata_names = Hashtbl.create 8
let strata_count = ref(-1)

let reset () = begin
  Hashtbl.reset state_names; state_count := -1;
  Hashtbl.reset strata_names; strata_count := -1
end

let rename names count i =
  if Hashtbl.mem names i then
    Hashtbl.find names i
  else begin
    incr count;
    Hashtbl.add names i (!count);
    !count
  end

let rename_state i = i (*rename state_names state_count*)
let rename_stratum = rename strata_names strata_count


(*
let min_models names node =
  if Cudd.is_true node then True
  else if Cudd.is_false node then False
  else
    let cubes = Cudd.bdd_cubes node in
    let remove_negative_states (i,l) v =
      let t = match v with
        | Cudd.True ->
          begin
            try Prop (Hashtbl.find names i) :: l
            with Not_found -> State i :: l
          end
        | Cudd.False ->
          begin
            try NProp (Hashtbl.find names i) :: l
            with Not_found -> l
          end
        | Cudd.Top -> l
      in
      (i+1, t)
    in
    let clean term =
      let (_,l) = Array.fold_left remove_negative_states (0,[]) term in
      List.rev l
    in
    Mod (List.map clean cubes)
*)

let rec print_list sep pp fmt = function
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a" pp x
  | x::xs ->
    Format.fprintf fmt "%a%a%a" pp x sep () (print_list sep pp) xs


let n = ref 0
let rec print_class ?(and_arrow=false) si fmt c =
  let open Ahw.Nfa.Label in
  let to_string' l =
    if is_true l then "" else " ["^to_string l^"] " in
  match c with
  | `Arrow(l,j) ->
    if not and_arrow && is_false l then ()
    else if is_true j then begin
      incr n;
      Format.fprintf fmt
        "true_%s_%d [shape=plaintext label=\"\"];@\n\
         %s -> true_%s_%d [label=\" %s \" arrowhead=dot];@\n"
        si !n si si !n (to_string' l)
    end
    else if is_state j then begin
      let j = get_state j in
      Format.fprintf fmt "%s -> %d [label=\" %s \"];@\n" si j (to_string' l)
    end
    else failwith
      (Printf.sprintf "Error: `Arrow(%s,%s)\n" (to_string l) (to_string j))

  | `OrArrow xs ->
    begin
      if and_arrow then begin
        incr n;
        let q = Printf.sprintf "disj_%s_%d" si !n in
        Format.fprintf fmt
          "%s [shape=invtriangle width=0.2 height=0.2 label=\"\" fixedsize=true];@\n\
          %s -> %s [label=\"\" arrowhead=none];@\n" q si q;
        List.iter (print_class q fmt) xs
      end
      else
       List.iter (print_class si fmt) xs
    end
  | `AndArrow (l,xs) ->
    incr n;
    let q = Printf.sprintf "conj_%s_%d" si !n in
    Format.fprintf fmt
      "%s [shape=triangle width=0.2 height=0.2 label=\"\" fixedsize=true];@\n\
      %s -> %s [label=\" %s \" arrowhead=none];@\n" q si q (to_string' l);
    List.iter (print_class ~and_arrow:true q fmt) xs

let ahw2dot mgr fmt ahw =
  reset ();

  let strata = Hashtbl.create 8 in

  let init =  get_init mgr ahw in

  (* Header *)
  Format.fprintf fmt "@[<v 1>digraph {@;@;rank = same;@;fontsize = 10;@;\
    labelloc=t;@;labeljust=l;@; \
    label = <@[<v 1><table border='0' cellspacing='0'>@; \
                    @[<v 1><tr>@; <td bgcolor='#94E9BF'> A=Accepting </td>@; \
                        <td bgcolor='#FF8383'> R=Rejecting </td>@; \
                        <td bgcolor='#99CCFF'> B=B&uuml;chi </td>@; \
                        <td bgcolor='#DADADA'> C=CoB&uuml;chi </td>@]@; \
                        <td bgcolor='#D494D4'> T=Transient </td>@]@; \
                    </tr>@; </table>@]>;@;@;";

  (* Print the initial condition *)
  (*let i = rename_state ahw in*)
  Format.fprintf fmt
    "start [shape=plaintext label=\"start\"];@\n";

  Format.fprintf fmt "%a" (print_class "start")
      (Ahw.Nfa.Label.classify init);

  (* BFS traversal of the AHW *)
  let waiting : state Queue.t = Queue.create () in
  let visited : (state,unit) Hashtbl.t = Hashtbl.create 8 in

  (* Add the initial state *)
  let initials = Misc.uniques (Ahw.Nfa.Label.states init) in
  List.iter (fun i ->
      Logger.debug ~level:10 "s:%d\n" i;
      Queue.add i waiting; Hashtbl.add visited i ()) initials;
  while not (Queue.is_empty waiting) do
    let q = Queue.take waiting in
    let i = rename_state q in
    let h = get_stratum mgr q in
    let strata_states =
      if Hashtbl.mem strata h then Hashtbl.find strata h
      else
        let t = Hashtbl.create 1 in Hashtbl.add strata h t; t in
    Hashtbl.add strata_states i ();

    (*assert (get_color mgr q >= 0 && get_color mgr q < 3);*)
    (* Print the node *)
    Format.fprintf fmt
      "%d [shape=%scircle fillcolor=white style=filled fixedsize=true];@\n"
      i (if is_final mgr q then "double" else "")(*(color.(get_color mgr q))*);

    (* Get successors condition *)
    let delta = Ahw.Nfa.Label.map_states (fun i ->
      Ahw.Nfa.Label.dstate (rename_state i)) (get_delta mgr q) in

    (* Print the condition *)


    (*Printf.eprintf "cond = %s -> %s\n"
      (Ahw.Nfa.Label.to_string delta)
      (Ahw.Nfa.Label.string_of_classified (Ahw.Nfa.Label.classify delta));
    *)

    Format.fprintf fmt "%a" (print_class (string_of_int i))
      (Ahw.Nfa.Label.classify delta);

    (* Equeue states that need to be proccessed *)
    Ahw.Nfa.Label.iter_states (fun i ->
      if not (Hashtbl.mem visited i) then begin
        Hashtbl.add visited i ();
        Queue.add i waiting
      end
    ) (get_delta mgr q)
  done;

  (* Create clusters *)
  Hashtbl.iter (fun h states ->
    let kind = stratum_kind mgr h in
    let hname = rename_stratum h in

    Format.fprintf fmt
      "@;@[<v 1>subgraph cluster_%d {@;\
       style=filled;@;\
       color=\"%s\";@;\
       node [style=filled,color=white];" hname (stratum_color mgr h);
    Hashtbl.iter (fun q _ -> Format.fprintf fmt "%d " q) states;
    Format.fprintf fmt ";@;labeljust=\"l\"@;label=%s;@]@;}@\n" kind;

  ) strata;

  (* Footer *)
  Format.fprintf fmt "@]@;}@."



let print_ahw mgr fmt ahw =
  reset();

  if Logger.level Logger.DEBUG = 10 then
    Ahw.print_manager mgr;

  (* Is the automaton very weak? *)
  Format.fprintf fmt "-- AHW is %svery weak.\n"
    (if Ahw.is_very_weak mgr ahw then "" else "NOT ");

  (* Print the initial state *)
  (*let init = get_init mgr ahw in*)
  Format.fprintf fmt "-- size: %d\n" (Ahw.size mgr ahw);
  Format.fprintf fmt "start: %s@\n" (Ahw.Nfa.Label.to_string (get_init mgr ahw));

  (* BFS traversal of the AHW *)
  let waiting : state Queue.t = Queue.create () in
  let visited : (state,unit) Hashtbl.t = Hashtbl.create 8 in

  (* Add the initial state *)
  let initials = Misc.uniques (Ahw.Nfa.Label.states (get_init mgr ahw)) in
  List.iter (fun i ->
    Logger.debug ~level:10 "s:%d\n" i;
    Queue.add i waiting; Hashtbl.add visited i ()) initials;
  while not (Queue.is_empty waiting) do
    let q = Queue.take waiting in
    let delta =
      Ahw.Nfa.Label.map_states (fun i -> Ahw.Nfa.Label.dstate (rename_state i))
        (get_delta mgr q)
    in
    let h = get_stratum mgr q in
    let hk = stratum_kind mgr h in
    let hs = get_stratum_size mgr ahw h in
    (*let h_states = get_stratum_states mgr h in*)
    Format.fprintf fmt "%4d [%d,%s,%d] : @\n"
      (rename_state q) (rename_stratum h) hk hs;
    List.iter (fun t ->
      Format.fprintf fmt "    :: %s @\n" (Ahw.Nfa.Label.to_string t))
      (Ahw.Nfa.Label.disj_list delta);
    Format.fprintf fmt "\n";
    Ahw.Nfa.Label.iter_states (fun i ->
      if not (Hashtbl.mem visited i) then begin
        Hashtbl.add visited i ();
        Queue.add i waiting
      end
    ) (get_delta mgr q)
  done;
  Format.fprintf fmt "@."


(*
  let print_ahw mgr fmt ahw =
  let cuddmgr = mgr.ahw_bddmgr.Bdd.bdd_mgr in
  let names = Bdd.hashed_vars mgr.ahw_bddmgr in
  let rename i = i in

  (* Print the initial state *)
  Format.fprintf fmt "start: %d@;" (rename (Cudd.index ahw));

  (* BFS traversal of the AHW *)
  let waiting : state Queue.t = Queue.create () in
  let visited : (state,unit) Hahstbl.t = Hashtbl.create 8 in

  (* Add the initial state *)
  Queue.add ahw waiting;
  Hashtbl.add visited ahw ();

  while not (Queue.is_empty waiting) do
    let q = Queue.take waiting in
    Format.fprintf fmt "%4d [%d] : "
      (rename (Cudd.index q)) (get_color mgr q);
    let succ = min_models names (get_delta mgr q) in
    match succ with
    | True -> Format.fprintf fmt "true@\n"
    | False -> Format.fprintf fmt "false@\n"
    | Mod ml -> Format.fprintf fmt "@[";
      begin
        let sep_and fmt () = Format.fprintf fmt " & " in
        let pp_and fmt = function
          | Prop s -> Format.fprintf fmt "%s" s
          | NProp s -> Format.fprintf fmt "!%s" s
          | State i ->
            let q = Cudd.ithvar cuddmgr i in
            if not (Hashtbl.mem visited q) then begin
              Hashtbl.add visited q ();
              Queue.add q waiting
            end;
            Format.fprintf fmt "%d" (rename (Cudd.index q))
        in
        let sep_or fmt () =  Format.fprintf fmt " | " in
        let pp_or fmt ls = print_list sep_and pp_and fmt ls in
        print_list sep_or pp_or fmt ml
      end;
      Format.fprintf fmt "@]@\n"
  done;
  Format.fprintf fmt "@."

  let print_conj fmt = function
  | [] -> Format.fprintf fmt "true"
  | l ->
  let sep_and fmt () = Format.fprintf fmt " &and; " in
  let pp_and fmt = function
  | Prop s -> Format.fprintf fmt "%s" s
  | NProp s -> Format.fprintf fmt "!%s" s
  | _ -> assert(false)
  in
  print_list sep_and pp_and fmt l

  let fprintf mgr fmt ahw =
  let cuddmgr = mgr.ahw_bddmgr.Bdd.bdd_mgr in
  let names =  Bdd.hashed_vars mgr.ahw_bddmgr in
  let rename i = i in

  (* Header *)
  Format.fprintf fmt
  "@[<v 1>digraph {@;@;rank = same;@;fontsize = 10;@;arrowType=open;@;@;";

  (* BFS traversal of the AHW *)
  let waiting = Queue.create () in
  let visited = Hashtbl.create 8 in

  (* Add the initial state *)
  Queue.add ahw waiting;
  Hashtbl.add visited ahw ();

  let name = rename (Cudd.index ahw) in
  Format.fprintf fmt
  "node_%d [shape=plaintext label=\"start\"]; \
  node_%d -> %d;@\n" name name name;

  while not (Queue.is_empty waiting) do
  let q = Queue.take waiting in
  let name = rename (Cudd.index q) in

  Format.fprintf fmt "%d [shape=circle color=%s fixedsize=true];@\n"
  name (color.(get_color mgr q));

  let succ = min_models names (get_delta mgr q) in
  let n = ref 0 in
  match succ with
  | True -> Format.fprintf fmt
  "true_%d [shape=plaintext label=\"\"];@\n \
  %d -> true_%d [label=\"[true]\" arrowhead=dot]@\n" name name name;
  | False -> ()
  | Mod ml -> List.iter (fun minterm ->
  begin
  let conj,states =
  List.fold_right (fun x (p,s) ->
  match x with
  | State i -> (p, i::s)
  | x -> (x::p, s)
  ) minterm ([],[])
  in
  if List.length states = 0 then begin
  Format.fprintf fmt
  "true_%d [shape=plaintext label=\"\"];@\n\
  %d -> true_%d [label=\" [%a] \" arrowhead=dot];@\n"
  name name name print_conj conj;
  end
  else begin
  List.iter (fun i ->
  let s = Cudd.ithvar cuddmgr i in
  if not (Hashtbl.mem visited s) then begin
  Hashtbl.add visited s ();
  Queue.add s waiting
  end
  ) states;

  if List.length states = 1 then begin
  let i = List.hd states in
  let sname = rename i in
  Format.fprintf fmt "%d -> %d [label=\" [%a] \"];@\n"
  name sname print_conj conj
  end
  else begin
  Format.fprintf fmt
  "conj_%d_%d [shape=point];@\n\
  %d -> conj_%d_%d [label=\" [%a]\" dir=none];@\n"
  name !n name name !n print_conj conj;

  List.iter (fun i ->
  Format.fprintf fmt "conj_%d_%d -> %d\n" name !n (rename i)
  ) states;

  incr n;

  end
  end
  end) ml;
  done;

  (* Footer *)
  Format.fprintf fmt "@]@;}@."
*)
