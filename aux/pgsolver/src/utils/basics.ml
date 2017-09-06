let message_ch = ref stdout

(* Verbosity levels: 0 = quiet, no messages at all
 *                   1 = normal, result and timing information
 *                   2 = alert, some more statistics
 *                   3 = debug, swamp the user *)
let verbosity = ref 1

type verbosity_level = int

let verbosity_level_verbose = 2

let verbosity_level_default = 3



let message u s = if !verbosity >= u then (output_string !message_ch (s ()); flush !message_ch)



let message_depth u depth s = message u (fun x -> (String.make (depth * 2) ' ') ^ s x)

let message_depth_tagged u depth tag s =
	message_depth u depth (fun x -> "[" ^ String.capitalize (tag x) ^ "]  " ^ s x)

let message_depth_counter = ref 0
let message_incrdepth _ = incr message_depth_counter
let message_decrdepth _ = decr message_depth_counter

let message_autod u = message_depth u !message_depth_counter

let message_autotagged u = message_depth_tagged u !message_depth_counter

let rec split_by_newline s =
	try
		let i = String.index s '\n' in
		let hd = String.sub s 0 (i + 1) in
		let tl = String.sub s (i + 1) (String.length s - i - 1) in
		hd::(split_by_newline tl)
	with Not_found ->
		if String.length s = 0 then [] else [s]

let message_autotagged_newline u tag s =
	if !verbosity >= u
	then List.iter (fun t -> message_autotagged u tag (fun _ -> t)) (split_by_newline (s ())) 
