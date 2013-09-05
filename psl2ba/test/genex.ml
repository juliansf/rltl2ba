(** Generates examples for testing correctness and performance of rtl2ba *)


(** Generates the formula switch_i from Example 2 *)
let switch (n:int) (i:int) : string =
	let s = ref "{!switch}*; switch" in
	for j=2 to n do
		s := (!s)^"; {!switch}*; switch"
	done;
	s := "{"^(!s)^"}*";
	for j=1 to i do
		s := (!s)^"; {!switch}*; switch "
	done;
	"{"^(!s)^"; {!switch}*}"


(** Generates the first formula of Example 2 *)
let gen_ex2_1 (n:int) (i:int) : string =
	"G(send -> {"^(switch n i)^" && {start; {!start}*} } <>->tt)"
	

(** Generates the second formula of Example 2 *)
let gen_ex2_2 (n:int) (i:int) : string =
	let union = ref "" in
	for j=0 to n do
		if j!=i then 
			if !union = "" then	union := switch n j   
			else union := (!union)^" || "^(switch n j)
	done; 
	"({{!start}*} <>-> send) | F(start & {{tt; {!start}*} && "^(!union)^"} <>-> send)" 





(** Generates the n-variable counter
		@param n must be at least 0 *)
let gen_countn (n:int) : string =
	(* All bits are 0 *)
	let f1 = 
		let s = ref "tt" in
		for i = 0 to n do
			s := !s^" & !c"^(string_of_int i)
		done;
		"("^(!s)^")"
	in
	(* Flip bits *)
	let f2 =
		let s = ref "tt" in
		for i = 1 to n do
			let si = string_of_int i in
			let sj = string_of_int (i-1) in
			s := !s^" & G(X (c"^si^") <-> (c"^si^" <-> (c"^sj^" -> X(c"^sj^"))))"
		done;
		"("^(!s)^")"
	in
	f1^" & G(X(!c0) <-> c0) & "^f2


(** Generates the 3-variable counter, see [Rozier, Vardi: SPIN'07]
		@param n must be at least 1 *)
let gen_count3 (n:int) : string =
	if n<1 then failwith "test: n must be at least 1";
	(* 1. The marker consists of a repeated pattern of a 1 followed by n-1 0's *)
	let rec f11 (i:int) : string = match i with
		| 1 -> "X(m)"
		| _ -> "X(!m & "^(f11 (i-1))^")"
	in 
	let f1 = "(m & G(m -> "^(f11 n)^"))" in
	(* 2. The first n bits are 0's *)
	let rec f21 (i:int) = match i with
		| 1 -> "!b"
		| _ -> "!b & X("^(f21 (i-1))^")"
	in 
	let f2 = "("^(f21 n)^")" in
	(* 3. If m=1 and b=0 then c=0 and n steps later b=1 *) 
	let rec f31 (i:int) = match i with
		| 1 -> "X(b)"
		| _ -> "X("^(f31 (i-1))^")"
	in
	let f3 = "G((m & !b) -> (!c & "^(f31 n)^"))" in
	(* 4. If m=1 and b=1 then c=1 and n steps later b=0 *) 
	let rec f41 (i:int) = match i with
		| 1 -> "X(!b)"
		| _ -> "X("^(f41 (i-1))^")"
	in
	let f4 = "G((m & b) -> (c & "^(f41 n)^"))" in
	(* 5. If c=0 then the next bit stays the same n steps later *)
	let f5 = "G((!c & X(!m)) -> (X(!c) & (X(b) -> "^(f31 (n+1))^") & (X(!b) -> "^(f41 (n+1))^")))" in
	(* 6. If c=0 then flip next bit n steps later and adjust carry *)
	let f6 = "G(c -> ((X(!b) -> (X(!c) & "^(f41 (n+1))^")) & (X(b) -> (X(c) & "^(f31 (n+1))^"))))" in
	f1^" & "^f2^" & "^f3^" & "^f4^" & "^f5^" & "^f6
	
	
	






let _ =
	(* parse command line arguments *)
	let mode = ref "" in
	let int1 = ref 1 in
	let int2 = ref 0 in
						
	let usage = "usage: "^Sys.argv.(0)^" [-options]\noptions:" in
	let speclist = [
		("-ex2.1", Arg.Tuple [Arg.Int (fun i -> mode:="-ex2.1"; int1:=i); Arg.Set_int(int2)], "<N> <i> : outputs formula 1 of example 2");
		("-ex2.2", Arg.Tuple [Arg.Int (fun i -> mode:="-ex2.2"; int1:=i); Arg.Set_int(int2)], "<N> <i> : outputs formula 2 of example 2");
		("-count3", Arg.Int (fun i -> mode:="-count3"; int1:=i), "<n> : outputs a 3-variable counter");
		("-countn", Arg.Int (fun i -> mode:="-countn"; int1:=i), "<n> : outputs a n-variable counter")]
	in
	Arg.parse 
		speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;

	match !mode with
		| "-ex2.1" ->
			print_endline (gen_ex2_1 !int1 !int2)
		| "-ex2.2" ->
			print_endline (gen_ex2_2 !int1 !int2)
		| "-count3" ->
			print_endline (gen_count3 !int1)
		| "-countn" ->
			print_endline (gen_countn !int1)
		| _ -> 
			Arg.usage speclist usage

