module Nfa = Nfa.Make(Bool.Default.B)
open Nfa

type desc =
  { header: Format.formatter -> unit -> unit;
    footer: Format.formatter -> unit -> unit;
    state: Format.formatter -> int * bool * bool -> unit;
    arrow: Format.formatter -> int * (Nfa.label * int) -> unit;
  }

type info = { size : int }

let print_scc fmt nfa =
  Array.iteri (fun sidx s ->

        Format.fprintf fmt "SCC(%d): " sidx;
        Array.iter (fun i ->
            Format.fprintf fmt "%d " i
          ) s;
        Format.fprintf fmt "\n";

    ) nfa.nfa_scc

let print_nfa print_label fmt nfa =
  let nfa =
    if nfa = Nfa.nfa_false then
      { nfa_delta=[|[]|]; nfa_start=0; nfa_final=[|false|]; nfa_scc=[|[|0|]|]; }
    else
      nfa
  in
  let n = size nfa in
  let rec print_delta fmt delta =
    match delta with
    | [] -> ()
    | [(b,i)] ->
      Format.fprintf fmt "@[%a -> %d@]" print_label b i
    | (b,i)::delta' ->
      Format.fprintf fmt "@[%a -> %d@\n%a@]"
        print_label b i print_delta delta'
  in
  for i=0 to n-1 do
    Format.fprintf fmt "@[%3d %s%s : @;<0 9>%a@]@."
      i
      (if i=nfa.nfa_start then "i" else " ")
      (if nfa.nfa_final.(i) then "f" else " ")
      print_delta nfa.nfa_delta.(i)
  done;
  print_scc fmt nfa

let raw_desc label info =
  let final = Array.create info.size false in
  let start = ref (-1) in
  let header fmt () = () in
  let footer fmt () = () in
  let state fmt (i,is_start,is_final) =
    if is_start then start := i;
    if is_final then final.(i) <- true
  in
  let curr = ref (-1) in
  let arrow fmt (i,(l,k)) =
    if not (i = !curr) then begin
      curr:=i;
      Format.fprintf fmt "%3d %s%s : %s -> %d"
        i
        (if i = !start then "i" else " ")
        (if final.(i) then "f" else " ")
        (label l) k
    end
    else
      Format.fprintf fmt "         %s -> %d" (label l) k
  in
  { header; footer; state; arrow }

let dot_desc label info =
  let header fmt () = Format.fprintf fmt
    "@[<v 1>digraph {@;@;rank = same;@;fontsize = 10;@;@;"
  in
  let footer fmt () = Format.fprintf fmt "@]@;}@." in
  let state fmt (i,start,final) =
    if start then Format.fprintf fmt
      "node_%d [shape=plaintext label=\"start\"]; \
       node_%d -> %d;@\n" i i i;
    Format.fprintf fmt "%d %s@\n" i
      (if final
       then "[shape=doublecircle color=green fixedsize=true];"
       else "[shape=circle color=red fixedsize=true];")
  in
  let arrow fmt (i,(l,k)) =
    Format.fprintf fmt "%d -> %d [label=\"%s\"];" i k (label l)
  in
  { header; footer; state; arrow }


let fprintf desc fmt nfa =
  let nfa =
    if nfa = Nfa.nfa_false then
      { nfa_delta=[|[]|]; nfa_start=0; nfa_final=[|false|]; nfa_scc=[|[|0|]|]; }
    else
      nfa
  in
  let n = size nfa in
  let desc = desc {size=n} in
  desc.header fmt ();
  for i=0 to n-1 do
    Format.fprintf fmt "@[%a@]@;"
      desc.state (i,(i=nfa.nfa_start),nfa.nfa_final.(i))
  done;
  for i=0 to n-1 do
    List.iter (fun x ->
      Format.fprintf fmt "@[%a@]@;" desc.arrow (i,x)
    ) nfa.nfa_delta.(i)
  done;
  desc.footer fmt ()
