module Nbw = Nbw.Make(Ahw.Make(Nfa.Make(Bool.Default.B)))
open Nbw

let state_is_true x = x.s = [||] && x.ok

let state_names : (state,int) Hashtbl.t = Hashtbl.create 8
let state_count = ref (-1)

let reset () = begin
  Hashtbl.reset state_names; state_count := -1;
end

let rename names count x =
  if Hashtbl.mem names x then
    Hashtbl.find names x
  else begin
    incr count;
    Hashtbl.add names x (!count);
    !count
  end

let rename_state = rename state_names state_count

let show_delta_dot istates node fmt dt =
  let module Label = Nbw.Ahw.Nfa.Label in
  let isize = Hashtbl.length istates in
  let {s;ok} = node in
  let shapecolor = if ok then "doublecircle color=green" else "circle" in
  let node_name = rename_state node in

  if Hashtbl.mem istates node then
    let idx =
      if isize > 1 then string_of_int node_name else "" in
    Format.fprintf fmt
      ("init%s [shape=%s fillcolor=lightgray "
       ^^ "style=filled width=0.9 fontsize=20];@;")
      idx shapecolor
  else
    Format.fprintf fmt
      "\"%d\" [shape=%s fillcolor=white style=filled width=0.7 fontsize=15];@;"
      node_name shapecolor;

  let print_init_node fmt x =
    let {s;ok} = x in
    if Hashtbl.mem istates x then
      let idx =
        if isize > 1 then string_of_int (rename_state x) else "" in
      Format.fprintf fmt "init%s" idx
    else
      Format.fprintf fmt "%d" (rename_state x) in

  Hashtbl.iter (fun node' l ->
    Format.fprintf fmt "\"%a\" -> \"%a\" [label=\" %s \" fontsize=16];@;"
      print_init_node node print_init_node node' (Label.to_string l)
  ) dt;
  Format.fprintf fmt "@;"

let nbw2dot mgr fmt nbwref =
  reset ();
  let nbw = find mgr nbwref in
  Format.fprintf fmt ("@[<v 1>digraph {@;@;rank = min;"
                      ^^"@;splines=true;@;fontsize = 10;"
                      ^^"@;fontname = \"serif\";@;@;");

  let visited = Hashtbl.create (Hashtbl.length nbw.nbw_delta) in
  let waiting = Queue.create () in

  Hashtbl.iter (fun node _ ->
    Queue.add node waiting
  ) nbw.nbw_init;

  while not (Queue.is_empty waiting) do
    let x = Queue.take waiting in
    if not (Hashtbl.mem visited x) then begin
      Hashtbl.add visited x ();
      let x_delta = Hashtbl.find nbw.nbw_delta x in
      Format.fprintf fmt "%a" (show_delta_dot nbw.nbw_init x) x_delta;
      Hashtbl.iter (fun y _ -> Queue.add y waiting) x_delta
    end
  done;

  Format.fprintf fmt "@]}@."




let show_delta_neverclaim accept_all istates node fmt dt =
  let module Label = Nbw.Ahw.Nfa.Label in
  let isize = Hashtbl.length istates in
  let {s;ok} = node in
  let node_name = rename_state node in

  let print_init_node fmt x =
    if accept_all && x = _true then
      Format.fprintf fmt "accept_all"
    else
      let accept = if x.ok then "accept" else "T0" in
      let node_name = rename_state x in
      if Hashtbl.mem istates x then
        if isize > 1 then
          Format.fprintf fmt "%s_S%d_init" accept node_name
        else Format.fprintf fmt "%s_init" accept
      else
        Format.fprintf fmt "%s_S%d" accept node_name
  in

  Format.fprintf fmt "@[<v 8>%a:@;" print_init_node node;

  Format.fprintf fmt "do@;";
  Hashtbl.iter (fun node' l ->
    let label = Label.to_string l in
    if state_is_true node' then
      Format.fprintf fmt ":: atomic { (%s) -> assert (!(%s)) }@;"
        label label
    else
      Format.fprintf fmt ":: (%s) -> goto %a@;" label  print_init_node node'
  ) dt;
  Format.fprintf fmt "od;@]@;"


let print_nbw mgr fmt nbwref =
  let module Label = Nbw.Ahw.Nfa.Label in
  reset ();
  let nbw = find mgr nbwref in

  Format.fprintf fmt "@[<v 0>never {@;";

  if is_false nbw then
    Format.fprintf fmt "@[<v 8>T0_init:@;false;@]@;"
  else if is_true nbw then begin
    let true_delta = Hashtbl.find nbw.nbw_delta _true in
    Format.fprintf fmt "%a" (show_delta_neverclaim false nbw.nbw_init _true) true_delta
  end
  else begin
    let print_true = Hashtbl.mem nbw.nbw_delta _true in

    let visited = Hashtbl.create (Hashtbl.length nbw.nbw_delta) in
    let waiting = Queue.create () in

    Hashtbl.iter (fun node _ ->
      Queue.add node waiting
    ) nbw.nbw_init;

    while not (Queue.is_empty waiting) do
      let x = Queue.take waiting in
      if not (Hashtbl.mem visited x || x = _true) then begin
        Hashtbl.add visited x ();
        let x_delta = Hashtbl.find nbw.nbw_delta x in
        Format.fprintf fmt "%a" (show_delta_neverclaim true nbw.nbw_init x) x_delta;
        Hashtbl.iter (fun y _ -> Queue.add y waiting) x_delta
      end
    done;

    if print_true then begin
      Format.fprintf fmt "@[<v 8>accept_all:@;skip@]@;";
    end
  end;

  Format.fprintf fmt "@]}@."
