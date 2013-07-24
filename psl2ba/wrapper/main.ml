(**	Wrapper to substitute ltl2smv with rtl2ba.
    Returns the content of the file "never.smv"
		
		usage: ltl2smv <number> <input-file> <output-file> 
		IMPORTANT: For windows the executable must be renamed to ltl2smv.exe *)



let _ =
	(*let ret = Sys.command ("cp never.smv "^Sys.argv.(3)) in*)
	let ic = open_in "never.smv" in
	let oc = open_out Sys.argv.(3) in
	output_string oc (Std.input_all ic)

(*				
  let phi = input_line (open_in Sys.argv.(2)) in
  let out = Sys.argv.(3) in
	
	let ret = Sys.command ("./rtl2ba -prefix '_"^Sys.argv.(1)^"' -smv '"^phi^"' > "^out) in

		let ret = Sys.command ("echo '"^phi^"' | ./modella -m > "^out) in
	*)
