let format_fprintf = Format.ifprintf;;
let format_eprintf fmt = Printf.ifprintf stderr fmt;;

module IS = Misc.IntSet
module IntSetHashtbl = Misc.SetHashtbl(Misc.IntSet)

  module type AtomType = sig
  type t
  val compare: t -> t -> int
  val to_string : t -> string
end

module type S = sig
  type elt
  type t

  val dtrue : t
  val dfalse : t

  val datom : elt -> t
  val dstate : int -> t
  val dnot : t -> t
  val dnot' : t -> t
  val dand : t -> t -> t
  val dand' : t -> t -> t
  val dor : t -> t -> t
  val dor' : t -> t -> t

  val simplify : t -> t
  val merge_conj : t -> t -> t list
  val merge_disj : t -> t -> t
  val propagate_disj : t -> t list -> t list * bool
  val nnf : t -> t
  val sop : t -> t
  val compose : t -> t -> int -> t (* for states *)
  val iter_states : (int -> unit) -> t -> unit
  val map_states : (int -> t) -> t -> t
  val states : t -> int list

  val classify :
    t ->
    ([ `AndArrow of
        t * [> `Arrow of t * t | `OrArrow of 'a list ] list
     | `Arrow of t * t
     | `OrArrow of 'a list ] as 'a)

  val string_of_classified :
    ([< `AndArrow of t * 'a list
     | `Arrow of t * t
     | `OrArrow of 'a list ]
        as 'a) ->
    string

  val disj_list : t -> t list
  val conj_list : t -> t list
  val merge_disj : t -> t -> t
  val merge_conj : t -> t -> t list

  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  val is_true : t -> bool
  val is_false : t -> bool
  val is_state : t -> bool

  val get_state : t -> int (* fails if not a state *)

  val arrows_product :
    t IntSetHashtbl.t -> t IntSetHashtbl.t -> t IntSetHashtbl.t
  val arrows : t -> t IntSetHashtbl.t

  val compare_bool : t -> t -> int
end

module Make(X : AtomType) : S with type elt = X.t =
struct

  module AtomSet = Set.Make(X)

  type elt = X.t

  type t =
  | True
  | False
  | Atom of elt
  | State of int
  | Not of t
  | And of t * t
  | Or of t * t

  let rec to_string = function
    | False -> "false"
    | True -> "true"
    | Atom s -> X.to_string s
    | State i -> string_of_int i
    | Not(And _ as x) | Not(Or _ as x) -> "!(" ^ to_string x ^ ")"
    | Not x -> "!" ^ to_string x
    | And ((Or _ as x), (Or _ as y)) ->
      "("^ to_string x ^") && ("^ to_string y ^")"
    | And (Or _ as x, y) -> "("^ to_string x ^") && "^ to_string y
    | And (x, (Or _ as y)) -> to_string x ^" && ("^ to_string y ^")"
    | And (x,y) -> to_string x ^ " && "^ to_string y
    | Or (x,y) ->  "("^to_string x ^") || ("^ to_string y^")"

  let print fmt l = format_fprintf fmt "%s" (to_string l)

  let compare_bool x y =
    match x,y with
    | Atom a, Not (Atom b)
    | Not (Atom a), Atom b -> compare a b
    | State i, State j -> compare i j
    | State _, _ -> 1
    | _, State _ -> -1
    | _ -> compare x y

  let (<) x y = compare_bool x y = -1

  let disj_list f =
    let rec disj_lst xs = function
      | Or(x,y) -> disj_lst (disj_lst xs x) y
      | x -> x::xs
    in
    disj_lst [] f

  let conj_list f =
    let rec conj_lst xs = function
      | And(x,y) -> conj_lst (conj_lst xs x) y
      | x -> x::xs
    in
    conj_lst [] f

  let disj_of_list = function
    | [] -> False
    | x::xs -> List.fold_left (fun p x -> Or(p,x)) x xs

  let dtrue = True
  let dfalse = False
  let datom x = Atom x
  let dstate x = State x
  let dnot x = match x with
    | True -> False
    | False -> True
    | State i -> State i
    | Not y -> y
    | x -> Not(x)
  let dand x y =
    match x,y with
    | x,y when x=y -> x
    | False,z | z,False -> False
    | True,z | z,True -> z
    | Not x, y when x=y -> False
    | y, Not x when x=y -> False
    | _ -> if x < y then And(x,y) else And(y,x)

  let dor x y = match x,y with
    | x,y when x=y -> x
    | False,z | z,False -> z
    | True,z | z,True -> True
    | Not x, y when x=y -> True
    | y, Not x when x=y -> True
    | Not x, Not y -> Not(And(x,y))
    | _ -> if x < y then Or (x,y) else Or(y,x)

  let rec dnot' x =
    let z = match x with
    | True -> False
    | False -> True
    | State i -> State i
    | Not y -> y
    | And(x,y) -> dor' (dnot' x) (dnot' y)
    | Or(x,y) -> dand' (dnot' x) (dnot' y)
    | x -> Not(x)
    in
    (*format_eprintf "########dnot'\nx=%s\n" (to_string x);
      format_eprintf "z=%s\n\n" (to_string z);*)
    z

  and dand' x y =
    let z = match x,y with
    | False,_ | _,False -> False
    | True,z | z,True -> z
    | Or(s,t), y | y, Or(s,t) -> dor' (dand' s y) (dand' t y)
    | (And _ as x),y | y,(And _ as x) ->
      let terms = List.fast_sort compare_bool ((conj_list x) @ (conj_list y)) in
      List.fold_right (fun x f ->
        match f with
        | True -> x
        | False -> False
        | _ -> match x with
          | True -> f
          | False -> False
          | _ -> And(x,f)
      ) (uniques False terms) True
    | x,y when x=y -> x
    | x,y -> dand x y
    in
    (*format_eprintf ":::::::::::dand'\nx=%s\ny=%s\n" (to_string x) (to_string y);
      format_eprintf "z=%s\n\n" (to_string z);*)
    z

  and  dor' x y =
    let z = match x,y with
    | True,_ | _,True -> True
    | False,z | z,False -> z
    | Atom p, Not(Atom q) | Not(Atom q), Atom p when p = q -> True
    | And(Atom p, Atom q), Atom r
    | Atom r, And(Atom p, Atom q) when p=r || q=r -> Atom r
    | x,y when x=y -> x
    | _ -> merge_disj x y
    in
    (*format_eprintf "++++++++++++++++dor'\nx=%s\ny=%s\n" (to_string x) (to_string y);
      format_eprintf "z=%s\n\n" (to_string z);*)
    z

  and uniques unit xs =
    let seen = Hashtbl.create (List.length xs) in
    try
      List.filter (fun x ->
        if not (Hashtbl.mem seen x) then
          if not (Hashtbl.mem seen (dnot' x)) then
            (Hashtbl.add seen x (); true)
          else raise Exit
        else false
      ) xs
    with Exit -> [unit]

  and merge_conj x y =
    (* We assume [x] and [y] are simplified conjunctions. *)
    format_eprintf "merge_conj :: x = %s;  y = %s\n" (to_string x) (to_string y);
    let xs,ys = conj_list x, conj_list y in
    let x,y,xs,ys = if List.length xs <= List.length ys
      then x,y,xs,ys else y,x,ys,xs in
    let t = Hashtbl.create (List.length ys) in
    List.iter (fun v -> if v != True then Hashtbl.add t v ()) ys;
    try List.iter (Hashtbl.find t) xs; [x]
    with Not_found -> if x < y then [x;y] else [y;x]

  and merge_disj x y =
    (* We assume [x] and [y] are both SOP *)
    format_eprintf "merge_disj :: x = %s;  y = %s\n" (to_string x) (to_string y);
    let xs,ys = disj_list x, disj_list y in
    List.iteri (fun i x -> format_eprintf "xs[%d] = %s\n" i (to_string x)) xs;
    List.iteri (fun i y -> format_eprintf "ys[%d] = %s\n" i (to_string y)) ys;
    let xs,ys = List.fold_right (fun w (zs,us) ->
        let (vs,seen) = propagate_disj w zs in
        if seen then (vs,us)
        else (vs, w::us)
      ) xs (ys,[]) in
    List.iteri (fun i x -> format_eprintf "xs[%d] = %s\n" i (to_string x)) xs;
    List.iteri (fun i y -> format_eprintf "ys[%d] = %s\n" i (to_string y)) ys;
    let x' = disj_of_list xs in
    let y' = disj_of_list ys in
    if x' = False then y' else if y' = False then x'
    else if x' = True || y' = True then True
    else if x' < y' then Or(x',y') else Or(y',x')


  and propagate_disj x xs =
    format_eprintf "propagate_disj::x = %s\n" (to_string x);
    List.iteri (fun i x -> format_eprintf "propagate_disj::xs[%d] = %s\n" i (to_string x)) xs;
    try (List.fold_right (fun y ys ->
        let mrg = merge_conj x y in
        if List.length mrg = 2 then y::ys
        else if List.hd mrg = x then ys
        else raise Exit
      ) xs [],false)
    with Exit -> (xs,true)

  let dor = dor'
  let dand = dand'
  let dnot = dnot'

  let rec to_string_smtlib = function
    | True -> "true"
    | False -> "false"
    | Atom s -> X.to_string s
    | State i -> string_of_int i
    | Not(And _ as x) | Not(Or _ as x) -> "~(" ^ to_string x ^ ")"
    | Not x -> "~" ^ to_string x
    | And ((Or _ as x), (Or _ as y)) ->
      "("^ to_string x ^") & ("^ to_string y ^")"
    | And (Or _ as x, y) -> "("^ to_string x ^") & "^ to_string y
    | And (x, (Or _ as y)) -> to_string x ^" & ("^ to_string y ^")"
    | And (x,y) -> to_string x ^" & "^ to_string y
    | Or (x,y) ->  to_string x ^" | "^ to_string y

  let rec classify f =
    let rec propagate b = function
      | `Arrow(l,i) -> `Arrow(dand' b l, i)
      | `AndArrow(l,xs) -> `AndArrow(dand' b l,xs)
      | `OrArrow xs -> `OrArrow (List.map (propagate b) xs)
    in
    let rec insert l i = function
      | [] -> [`Arrow(l,i)]
      | `Arrow (l',i') :: xs when i=i' -> `Arrow(dor' l l', i) :: xs
      | x :: xs -> x :: insert l i xs
    in

    match f with
    | True | False | Atom _ | Not _ -> `Arrow (f, True)
    | State i -> `Arrow(True, State i)
    | And(x,y) ->
      begin
        match classify x, classify y with
        | `Arrow(l,i), `Arrow(l',j)  ->
          if i=True then `Arrow(dand' l l',j)
          else if j=True || i=j then `Arrow(dand' l l',i)
          else `AndArrow(dand' l l',
            [`Arrow(True,i); `Arrow(True,j)])

        | `Arrow(l,i), `AndArrow(l',xs)
        | `AndArrow(l',xs), `Arrow(l,i) ->
          let xs' = if i = True then xs else `Arrow(True,i)::xs in
          `AndArrow(dand' l l', xs')

        | `Arrow(l,i), `OrArrow xs
        | `OrArrow xs, `Arrow(l,i) ->
          if i=True then propagate l (`OrArrow xs)
          else `AndArrow(l, [`Arrow(True,i);`OrArrow xs])

        | `AndArrow(l,xs), `AndArrow(l',xs') -> `AndArrow(dand' l l', xs@xs')

        | `AndArrow(l,xs), `OrArrow xs'
        | `OrArrow xs', `AndArrow(l,xs) ->
          `AndArrow(l, `OrArrow xs'::xs)

        | `OrArrow xs, `OrArrow xs' ->
          `AndArrow(True, [`OrArrow xs ;`OrArrow xs'])
      end
    | Or(x,y) ->
      begin
        match classify x, classify y with
        | `Arrow(l,i), `Arrow(l',j)  ->
          if i=j then `Arrow(dor' l l',i)
          else `OrArrow [`Arrow(l,i);`Arrow(l',j)]

        | `Arrow(l,i), `OrArrow xs
        | `OrArrow xs, `Arrow(l,i) -> `OrArrow (insert l i xs)

        | `OrArrow xs, `OrArrow xs' -> `OrArrow(xs@xs')

        | x,y -> `OrArrow [x;y]
      end

  let rec string_of_classified = function
    | `Arrow(l,i) -> Printf.sprintf "Arrow(%s,%s)" (to_string l) (to_string i)
    | `AndArrow(l,xs) -> Printf.sprintf "AndArrow(%s,[%s])" (to_string l)
      (List.fold_left (fun s x -> s^"_"^(string_of_classified x)^"_") "" xs)
    | `OrArrow xs -> Printf.sprintf "OrArrow(%s)"
      (List.fold_left (fun s x -> s^"_"^(string_of_classified x)^"_") "" xs)

  (*
    let rec merge xs ys xflag yflag xys =
    match x,y with
    | [],[] -> (* ... produce the formula ... *)
    | z::zs, []
    | [], z::zs -> merge xs [] xflag (yflag&&false) ((z,False)::xys)
    | x::xs, y::ys ->
      if x < y -> merge xs (y::ys) xflag (yflag&&false) ((x,False)::xys
  *)



  let rec nnf = function
    | Not(True) -> False
    | Not(False) -> True
    | Not(State i) -> State i
    | Not(Not x) -> nnf(x)
    | Not(And(x,y)) -> Or(nnf(Not(x)), nnf(Not(y)))
    | Not(Or(x,y)) -> And(nnf(Not(x)),nnf(Not(y)))
    | And(x,y) -> And(nnf x, nnf y)
    | Or(x,y) -> Or(nnf x, nnf y)
    | x -> x

  let rec sop = function
    | And(Or(x,y),z)
    | And(z,Or(x,y)) -> Or(sop (And(x,z)), sop (And(y,z)))
    | And(x,y) -> begin
      match sop x with
      | Or(u,v) -> Or (sop (And(u,y)), sop (And(v,y)))
      | x -> begin
        match sop y with
        | Or(u,v) -> Or (sop (And(u,x)), sop (And(v,x)))
        | y -> And (x,y)
      end
    end
    | Or(x,y) -> Or(sop x, sop y)
    | Not x as f -> (match nnf f with Not u -> Not u | u -> sop u)
    | x -> x

  (*let reduce f =
    let sop_f = sop f in
    let sop_l = List.map conj_list (disj_list sop_f) in
    let terms = Hashtbl.create 8 in
    let head = List.hd sop_l *)

  let rec reorder f =
    let rec join_and = function
      | [] -> True
      | [x] -> x
      | x::xs -> And(x,join_and xs)
    in
    let rec join_or = function
      | [] -> False
      | [x] -> x
      | x::xs -> Or(x,join_or xs)
    in
    let f' = nnf f in
    match f' with
    | Not _ | Atom _ | State _ | True | False as x -> x
    | And (x,y) ->
      let x' = reorder x in
      let y' = reorder y in
      if x' = y' then x' else begin
        let conjuncts = uniques False (conj_list (And(x',y'))) in
        match conjuncts with
        | [] -> failwith "__file__:__line__: [internal error] cannot ocurr."
        | xs -> join_and (List.fast_sort Pervasives.compare xs)
      end
    | Or (x,y) ->
      let x' = reorder x in
      let y' = reorder y in
      if x' = y' then x' else begin
        let disjuncts = uniques True (disj_list (Or(x',y'))) in
        match disjuncts with
        | [] -> failwith "__file__:__line__: [internal error] cannot ocurr."
        | xs -> join_or (List.fast_sort Pervasives.compare xs)
      end

  let rec simplify f =
    let f' = match reorder f with
    | Not x ->
      begin
        let x' = simplify x in
        match x' with
        | True -> False
        | False -> True
        | Not y -> y
        | Or (Not x, Not y) -> simplify (And(x,y))
        | And (Not x, Not y) -> simplify (Or(x,y))
        | _ -> Not x'
      end
    | And(x,y) ->
      begin
        let x' = simplify x in
        let y' = simplify y in
        if x' = y' then x'
        else
          match x',y' with
          | False,z | z,False -> False
          | True,z | z,True -> z
          | Or(x,y), Or(v,w) ->
            if x=v then simplify (And(x,Or(y,w)))
            else if x=w then simplify (And(x,Or(y,v)))
            else if y=v then simplify (And(y,Or(x,w)))
            else if y=w then simplify (And(y,Or(x,v)))
            else And(x',y')
          | x, And(y,z) | And(y,z),x ->
            if x=y || x=z then And(y,z)
            else And(x,And(y,z))
          | x, Or(y,z) | Or(y,z), x ->
            if x=y || x=z then x
            else And(x,Or(y,z))
          | _ -> if x' < y' then And(x',y') else And(y',x')
      end
    | Or(x,y) ->
      begin
        let x' = simplify x in
        let y' = simplify y in
        if x' = y' then x'
        else
          match x',y' with
          | True,z | z,True -> True
          | False,z | z,False -> z
          | x, And(y,z) | And(y,z), x ->
            if x=y || x=z then x
            else Or(x,And(y,z))
          | x, Or(y,z) | Or(y,z), x ->
            if x=y || x=z then Or(y,z)
            else Or(x,Or(y,z))
          | _ -> if x' < y' then Or(x',y') else Or(y',x')
      end
    | x -> x
    in
    f'

  let simplify f = f

  let rec iter_states fn f =
    match f with
    | State x -> fn x
    | Not x -> iter_states fn x
    | And(x,y) -> iter_states fn x; iter_states fn y
    | Or(x,y) -> iter_states fn x; iter_states fn y
    | x -> ()


  let states f =
    let t = Hashtbl.create 8 in
    iter_states (fun x -> Hashtbl.add t x ()) f;
    Hashtbl.fold (fun x _ xs -> x::xs) t []

  let rec map_states fn f =
    match f with
    | State x -> fn x
    | Not x ->
      begin
      match map_states fn x with
      | True -> False
      | False -> True
      | x -> dnot x
      end
    | And(x,y) ->
      begin
        match map_states fn x, map_states fn y with
        | True,z | z,True -> z
        | False,z | z,False -> False
        | x,y -> dand x y
      end
    | Or(x,y) ->
      begin
        match map_states fn x, map_states fn y with
        | True,z | z,True -> True
        | False,z | z,False -> z
        | x,y -> dor x y
      end
    | x -> x

  let rec compose f g i =
    map_states (fun x -> if x=i then g else State x) f

  let mgr = Bdd.init ()
  let bstates = Hashtbl.create 8
  let nodes = Hashtbl.create 8

  let toBdd f =
    (*format_fprintf stderr "toBdd...%!";*)
    let rec toBdd' = function
      | True -> Cudd.dtrue mgr.Bdd.bdd_mgr
      | False -> Cudd.dfalse mgr.Bdd.bdd_mgr
      | Atom s -> Bdd.named_var mgr (X.to_string s)
      | State i ->
        if Hashtbl.mem bstates i then
          Hashtbl.find bstates i
        else begin
          let x = Cudd.newvar mgr.Bdd.bdd_mgr in
          Hashtbl.add bstates i x;
          x
        end
      | Not x -> (*format_fprintf stderr "not %!";*)Cudd.dnot (toBdd' x)
      | And(x,y) -> (*format_fprintf stderr "and %!";*)Cudd.dand (toBdd' x) (toBdd' y)
      | Or(x,y) -> (*format_fprintf stderr "or %!";*)Cudd.dor (toBdd' x) (toBdd' y)
    in
    let x =
      if Hashtbl.mem nodes f then
        Hashtbl.find nodes f, true
      else begin
        let res = toBdd' f in
        Hashtbl.add nodes f res;
        res, false
      end
    in x
    (*format_fprintf stderr " done\n%!";*)
    (*toBdd' f, false*)

  let n = ref 0
  let is_true f =
    (*format_fprintf stderr "is_true?(%s)\n%!" (to_string f);*)
    try
      iter_states (fun _ -> assert false) f;
      let res,bdd_check =
        match f with
        | True | Not(False) -> true,"NO"
        | False | Atom _ | State _
        | Not(True) | Not(Atom _) | Not(State _)-> false,"NO"
        | _ -> let node,cached = toBdd f in
               Cudd.is_true node,
               "YES ["^if cached then "cached]" else "uncached]"
      in
      (*format_eprintf "n: %d :: is_true(%s) (BDD check: %s)\n"
        !n (to_string f) bdd_check;*)
      incr n;
      res
    with _ -> false

  let is_false f =
    (*format_fprintf stderr "is_false?(%s)\n%!" (to_string f);*)
    try
      iter_states (fun _ -> assert false) f;
      let res,bdd_check =
        match f with
        | False | Not(True)-> true,"NO"
        | True | Atom _ | State _
        | Not(False) | Not(Atom _) | Not(State _)-> false,"NO"
        | _ -> let node,cached = toBdd f in
               Cudd.is_false node,
               "YES ["^if cached then "cached]" else "uncached]"
      in
      (*format_eprintf "n: %d :: is_false(%s) (BDD check: %s)\n"
        !n (to_string f) bdd_check;*)
      incr n;
      res
    with _ -> false

  let is_state = function
    | State _ -> true
    | _ -> false

  let get_state = function
    | State i -> i
    | f -> failwith ("Not a state: [" ^ to_string f ^ "]\n")

(*
  let rec is_true = function
    | True -> true
    | False -> false
    | Atom s -> false
    | Not x -> is_false x
    | And(False, x) | And (x, False) -> false
    | And(True, x) | And (x, True) -> is_true x
    | And(x,y) -> is_true x && is_true y
    | Or(True, x) | Or (x, True) -> true
    | Or(False, x) | Or (x, False) -> is_true x
    | Or(x,y) -> is_true x || is_true y

  and is_false = function
    | True -> false
    | False -> true
    | Atom s -> false
    | Not x -> is_true x
    | And(False, x) | And (x, False) -> true
    | And(True, x) | And (x, True) -> is_false x
    | And(x,y) -> is_false x || is_false y
    | Or(True, x) | Or (x, True) -> false
    | Or(False, x) | Or (x, False) -> is_false x
    | Or(x,y) -> is_false x && is_false y
*)

  let printset is =
    format_eprintf "{ ";
    IS.iter (fun i -> format_eprintf "%d " i) is;
    format_eprintf "}"

  let arrows_product xt yt =
    let res = IntSetHashtbl.create
      (IntSetHashtbl.length xt * IntSetHashtbl.length yt) in

    IntSetHashtbl.iter (fun ix lx ->
      IntSetHashtbl.iter (fun iy ly ->
        let is = IS.union ix iy in
        (*format_eprintf "lx = %s;  ly = %s\n" (to_string lx) (to_string ly);*)
        let l = dand' lx ly in
        let l' = try IntSetHashtbl.find res is with Not_found -> False in
        (*format_eprintf "BEFORE: **%s** **%s**\n" (to_string l) (to_string l');*)
        let label = dor' l l' in
        (*format_eprintf "AFTER:  %s\n\n" (to_string label);*)

        if not (is_false label) then
          IntSetHashtbl.replace res is label
      ) yt;
    ) xt;
    res

  let arrows f =
    let new_pair l x =
      let t = IntSetHashtbl.create 1 in
      IntSetHashtbl.add t x l; t in
    let merge xt yt =
      let res, src =
        if (IntSetHashtbl.length xt) - (IntSetHashtbl.length yt) >= 0 then xt, yt
        else yt, xt in
      IntSetHashtbl.iter (fun i l ->
        let l' = try IntSetHashtbl.find res i with Not_found -> False in
        IntSetHashtbl.replace res i (dor' l l')
      ) src;
      res
    in
    let rec compute = function
      | True -> new_pair True IS.empty
      | False -> IntSetHashtbl.create 0
      | State i -> new_pair True (IS.singleton i)
      | Atom x -> new_pair (Atom x) IS.empty
      | Not (Atom x) -> new_pair (Not (Atom x)) IS.empty
      | Not _ -> failwith "__file__:__line__: [internal error] cannot ocurr."
      | And (x, y) -> arrows_product (compute x) (compute y)
      | Or (x,y) -> merge (compute x) (compute y)
    in
    compute (nnf f)
end

module Default = struct
  module Atoms = struct
    include String
    let to_string s = s
  end
  module B = Make(Atoms)
end


(*
  #directory "_build/lib/cudd";;
  #directory "_build/src/rltl/automata/";;
  #directory "_build/src/utils/";;
  #load "cudd.cma";;
  #load_rec "bool.cmo";;
  #load "str.cma";;

  open Bool.Default.B;;
  open Misc;;

  let show_arrows at =
    Bool.IntSetHashtbl.iter (fun is l ->
      Printf.printf "{ ";
      IntSet.iter (fun i -> Printf.printf "%d " i) is;
      Printf.printf "} <- %s\n" (to_string l);
    ) at;;

  let symbols s = List.map datom (split (regexp "[ \t]+") s);;

  let sop_list = List.map (List.map to_string);;
  let sop_l g = List.map conj_list (disj_list g);;

  let s1 = dstate 1;;
  let s2 = dstate 2;;
  let s3 = dstate 3;;
  let s4 = dstate 4;;
  let s5 = dstate 5;;

  let w = datom "a";;
  let x = datom "b";;
  let y = datom "c";;
  let z = datom "d";;

  let [w;x;y;z] = symbols("w x y z");;
  let [a;b;c;d;e;f;g] = symbols("a b c d e f g");;

  let f = dor (dand s2 (dor x s3)) s1;;
  let a = arrows f;;

  let ($|) x y = dor' x y;;
  let ($&) x y = dand' x y;;

  let g = x $| (x $& z) $| (x $& y $& z) $| (x $& y);;
  let g1 = (w $& x $& z) $| (x $& y $& z);;

  let h1 = y $| (s1 $& x $& (w $| s2));;
  let h2 = (x $& (w $| s2)) $| (s3 $& z);;
  let h3 = (s1 $& x) $| (s2 $& z);;

  let a1 = arrows h1;;
  let a2 = arrows h2;;
  let a3 = arrows h3;;
  let a12 = arrows_product a1 a2;;
  let a123 = arrows_product a12 a3;;


  let open = datom "opened";;
  let call = datom "call";;
  let at_floor = datom "at_floor";;
  let s7 = dstate 7;;
  let s8 = dstate 8;;
  let s9 = dstate 9;;
  let s13 = dstate 13;;
  let f2 = (opened $| (dnot call) $| (dnot opened $& s3)  $| (at_floor $& (dnot opened) $& s8) $| ((dnot at_floor) $& (dnot opened) $& s9) $| ((dnot at_floor) $& (dnot opened) $& s7)) $& s5;;

*)
