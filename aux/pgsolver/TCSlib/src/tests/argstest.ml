open Tcsargs;;

let out s = print_string s;;

let _ =
	let args_obj = CustomArgs.new_object "Args Title" "Main Help String" true ["/"] in
	
	let _ = CustomArgs.register_argument args_obj None true true None ["test"] "Test Help" (fun _ -> "Long Help") (CustomArgs.Int (fun s -> out ("Test '" ^ string_of_int s ^ "' Selected\n"))) in

	let _ = CustomArgs.register_help_function args_obj ["help"] "Help String" (fun _ -> "Long Help") out in
	
	let _ = CustomArgs.register_argument args_obj None true true None ["dummy"] "Dummy Help" (fun _ -> "Long Dummy") (CustomArgs.Int (fun s -> out ("Dummy '" ^ string_of_int s ^ "' Selected\n"))) in

	CustomArgs.parse_arguments_with_error args_obj Sys.argv 1 (CustomArgs.error_function args_obj out);
	
	();;