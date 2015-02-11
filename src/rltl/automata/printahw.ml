module Ahw = Ahw.Make(Nfa.Make(Bool.Default.B))
open Ahw

let color = [|"gray"; "red"; "green"|]

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
          "%s [shape=invtriangle width=0.2 height=0.2 label=\"\" fixedsize];@\n\
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
      "%s [shape=triangle width=0.2 height=0.2 label=\"\" fixedsize];@\n\
      %s -> %s [label=\" %s \" arrowhead=none];@\n" q si q (to_string' l);
    List.iter (print_class ~and_arrow:true q fmt) xs

let ahw2dot mgr fmt ahw =
  let state_names = Hashtbl.create 8 in
  let state_count = ref (-1) in
  let rename i =
    if Hashtbl.mem state_names i then
      Hashtbl.find state_names i
    else begin
      incr state_count;
      Hashtbl.add state_names i (!state_count);
      !state_count
    end
  in

  (* Header *)
  Format.fprintf fmt "@[<v 1>digraph {@;@;rank = same;@;fontsize = 10;@;@;";

  (* Print the initial state *)
  let i = rename ahw in
  Format.fprintf fmt
    "node_%d [shape=plaintext label=\"start\"]; \
     node_%d -> %d;@\n" i i i;

  (* BFS traversal of the AHW *)
  let waiting : state Queue.t = Queue.create () in
  let visited : (state,unit) Hashtbl.t = Hashtbl.create 8 in

  (* Add the initial state *)
  Queue.add ahw waiting;
  Hashtbl.add visited ahw ();
  while not (Queue.is_empty waiting) do
    let q = Queue.take waiting in
    let i = rename q in
    assert (get_color mgr q >= 0 && get_color mgr q < 3);
    (* Print the node *)
    Format.fprintf fmt "%d [shape=circle color=%s fixedsize=true];@\n"
      i (color.(get_color mgr q));

    (* Get successors condition *)
    let delta = Ahw.Nfa.Label.map_states (fun i ->
      Ahw.Nfa.Label.dstate (rename i)) (get_delta mgr q) in

    (* Print the condition *)
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

  (* Footer *)
  Format.fprintf fmt "@]@;}@."



let print_ahw mgr fmt ahw =
  let state_names = Hashtbl.create 8 in
  let state_count = ref (-1) in
  let rename i = i
(*    if Hashtbl.mem state_names i then
      Hashtbl.find state_names i
    else begin
      incr state_count;
      Hashtbl.add state_names i (!state_count);
      !state_count
    end*)
  in

  (* Print the initial state *)
  Format.fprintf fmt "start: %d@;" (rename ahw);

  (* BFS traversal of the AHW *)
  let waiting : state Queue.t = Queue.create () in
  let visited : (state,unit) Hashtbl.t = Hashtbl.create 8 in

  (* Add the initial state *)
  Queue.add ahw waiting;
  Hashtbl.add visited ahw ();
  while not (Queue.is_empty waiting) do
    let q = Queue.take waiting in
    let delta =
      Ahw.Nfa.Label.map_states (fun i -> Ahw.Nfa.Label.dstate (rename i))
        (get_delta mgr q)
    in
    Format.fprintf fmt "%4d [%d] : " (rename q) (get_color mgr q);
    Format.fprintf fmt "%s@\n" (Ahw.Nfa.Label.to_string delta);
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
