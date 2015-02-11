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
  val dand : t -> t -> t
  val dor : t -> t -> t

  val simplify : t -> t
  val nnf : t -> t
  val compose : t -> t -> int -> t (* for states *)
  val iter_states : (int -> unit) -> t -> unit
  val map_states : (int -> t) -> t -> t

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

  val to_string : t -> string

  val is_true : t -> bool
  val is_false : t -> bool
  val is_state : t -> bool

  val get_state : t -> int (* fails if not a state *)
end

module Make(X : AtomType) : S with type elt = X.t =
struct
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

  let dtrue = True
  let dfalse = False
  let datom x = Atom x
  let dstate x = State x
  let dnot x = match x with
    | True -> False
    | False -> True
    | Not y -> y
    | x -> Not(x)
  let dand x y =
    match x,y with
    | False,z | z,False -> False
    | True,z | z,True -> z
    | Not x, Not y -> Not(Or(x,y))
    | _ -> And(x,y)
  let dor x y = match x,y with
    | False,z | z,False -> z
    | True,z | z,True -> True
    | Not x, Not y -> Not(And(x,y))
    | _ -> Or (x,y)

  let disj_list f =
    let rec disj_lst xs f =
      match f with
      | Or(x,y) -> disj_lst (disj_lst xs x) y
      | x -> x::xs
    in
    disj_lst [] f

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
      | `Arrow(l,i) -> `Arrow(dand b l, i)
      | `AndArrow(l,xs) -> `AndArrow(dand b l,xs)
      | `OrArrow xs -> `OrArrow (List.map (propagate b) xs)
    in
    match f with
    | True | False | Atom _ | Not _ -> `Arrow (f, True)
    | State i -> `Arrow(True, State i)
    | And(x,y) ->
      begin
        match classify x, classify y with
        | `Arrow(l,i), `Arrow(l',j)  ->
          if i=True then `Arrow(dand l l',j)
          else if j=True || i=j then `Arrow(dand l l',i)
          else `AndArrow(dand l l',
            [`Arrow(True,i); `Arrow(True,j)])

        | `Arrow(l,i), `AndArrow(l',xs)
        | `AndArrow(l',xs), `Arrow(l,i) ->
          `AndArrow(dand l l', `Arrow(True,i)::xs)

        | `Arrow(l,i), `OrArrow xs
        | `OrArrow xs, `Arrow(l,i) ->
          if i=True then propagate l (`OrArrow xs)
          else `AndArrow(l, [`Arrow(True,i);`OrArrow xs])

        | `AndArrow(l,xs), `AndArrow(l',xs') -> `AndArrow(dand l l', xs@xs')

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
          if i=j then `Arrow(dand l l',i)
          else `OrArrow [`Arrow(l,i);`Arrow(l',j)]

        | `OrArrow xs, `OrArrow xs' -> `OrArrow(xs@xs')

        | x,y -> `OrArrow [x;y]
      end

  let rec string_of_classified = function
    | `Arrow(l,i) -> Printf.sprintf "Arrow(%s,%s)" (to_string l) (to_string i)
    | `AndArrow(l,xs) -> Printf.sprintf "AndArrow(%s,[%s])" (to_string l)
      (List.fold_left (fun s x -> s^"_"^(string_of_classified x)^"_") "" xs)
    | `OrArrow xs -> Printf.sprintf "OrArrow(%s)"
      (List.fold_left (fun s x -> s^"_"^(string_of_classified x)^"_") "" xs)

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
      let rec conj_lst = function
        | And (a,b) -> (conj_lst a) @ (conj_lst b)
        | v -> [v]
      in
      let x' = reorder x in
      let y' = reorder y in
      if x' = y' then x' else begin
        let conjuncts = Misc.uniques (conj_lst (And(x',y'))) in
        match conjuncts with
        | [] -> failwith "__file__:__line__: [internal error] cannot ocurr."
        | xs -> join_and xs
      end
    | Or (x,y) ->
      let rec disj_lst = function
        | Or (a,b) -> (disj_lst a) @ (disj_lst b)
        | v -> [v]
      in
      let x' = reorder x in
      let y' = reorder y in
      if x' = y' then x' else begin
        let disjuncts = Misc.uniques (disj_lst (Or(x',y'))) in
        match disjuncts with
        | [] -> failwith "__file__:__line__: [internal error] cannot ocurr."
        | xs -> join_or xs
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
        | Or (Not x, Not y) -> And(x,y)
        | And (Not x, Not y) -> Or(x,y)
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
          | Not x, Not y -> Not(Or(x,y))
          | Or(x,y), Or(v,w) ->
            if x=v then And(x,Or(y,w))
            else if x=w then And(x,Or(y,v))
            else if y=v then And(y,Or(x,w))
            else if y=w then And(y,Or(x,v))
            else And(x',y')
          | x, And(y,z) | And(y,z),x ->
            if x=y || x=z then And(y,z)
            else And(x,And(y,z))
          | x, Or(y,z) | Or(y,z), x ->
            if x=y || x=z then x
            else And(x,Or(y,z))
          | _ -> And(x',y')
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
          | Not x, Not y -> Not(And(x,y))
          | x, And(y,z) | And(y,z), x ->
            if x=y || x=z then x
            else Or(x,And(y,z))
          | x, Or(y,z) | Or(y,z), x ->
            if x=y || x=z then Or(y,z)
            else Or(x,Or(y,z))
          | _ -> Or(x',y')
      end
    | x -> x
    in
    f'

  let rec iter_states fn f =
    match f with
    | State x -> fn x
    | Not x -> iter_states fn x
    | And(x,y) -> iter_states fn x; iter_states fn y
    | Or(x,y) -> iter_states fn x; iter_states fn y
    | x -> ()

  let rec map_states fn f =
    match f with
    | State x -> fn x
    | Not x ->
      begin
      match map_states fn x with
      | True -> False
      | False -> True
      | x -> Not(x)
      end
    | And(x,y) ->
      begin
        match map_states fn x, map_states fn y with
        | True,z | z,True -> z
        | False,z | z,False -> False
        | x,y -> And(x,y)
      end
    | Or(x,y) ->
      begin
        match map_states fn x, map_states fn y with
        | True,z | z,True -> True
        | False,z | z,False -> z
        | x,y -> Or(x,y)
      end
    | x -> x

  let rec compose f g i =
    map_states (fun x -> if x=i then g else State x) f
(*
    match f with
    | State x when x = i -> g
    | Not x -> Not (compose x g i)
    | And(x,y) -> And(compose x g i, compose y g i)
    | Or(x,y) -> Or(compose x g i, compose y g i)
    | x -> x
*)

  let mgr = Bdd.init ()
  let states = Hashtbl.create 8
  let nodes = Hashtbl.create 8

  let toBdd f =
    (*Printf.fprintf stderr "toBdd...%!";*)
    let rec toBdd' = function
      | True -> Cudd.dtrue mgr.Bdd.bdd_mgr
      | False -> Cudd.dfalse mgr.Bdd.bdd_mgr
      | Atom s -> Bdd.named_var mgr (X.to_string s)
      | State i ->
        if Hashtbl.mem states i then
          Hashtbl.find states i
        else begin
          let x = Cudd.newvar mgr.Bdd.bdd_mgr in
          Hashtbl.add states i x;
          x
        end
      | Not x -> (*Printf.fprintf stderr "not %!";*)Cudd.dnot (toBdd' x)
      | And(x,y) -> (*Printf.fprintf stderr "and %!";*)Cudd.dand (toBdd' x) (toBdd' y)
      | Or(x,y) -> (*Printf.fprintf stderr "or %!";*)Cudd.dor (toBdd' x) (toBdd' y)
    in
    let x =
      if Hashtbl.mem nodes f then
        Hashtbl.find nodes f, true
      else begin
        let res = toBdd' f in
        Hashtbl.add nodes f res;
        res, false
      end
    in
    (*Printf.fprintf stderr " done\n%!";*)
    toBdd' f, false

  let n = ref 0
  let is_true f =
    (*Printf.fprintf stderr "is_true?(%s)\n%!" (to_string f);*)
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
      (*Printf.eprintf "n: %d :: is_true(%s) (BDD check: %s)\n"
        !n (to_string f) bdd_check;*)
      incr n;
      res
    with _ -> false

  let is_false f =
    (*Printf.fprintf stderr "is_false?(%s)\n%!" (to_string f);*)
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
      (*Printf.eprintf "n: %d :: is_false(%s) (BDD check: %s)\n"
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
end

module Default = struct
  module B = Make(struct include String let to_string s = s end)
end
