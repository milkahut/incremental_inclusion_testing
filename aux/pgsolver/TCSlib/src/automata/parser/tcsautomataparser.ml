open Tcsautomataparserinternal;;
open Tcsautomata;;
open Tcsset;;

exception AutomataParserException of string * int * int

exception AutomataParserCustomException of string

let parse_parity_game has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__pg_has_header := __def_pg_has_header;
		__pg_has_start := __def_pg_has_start;
		__pg_add_node := __def_pg_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (AutomataParserException (s, l, c))
	);
	__pg_has_header := has_header;
	__pg_has_start := has_start;
	__pg_add_node := add_func;
	(try
		Tcsparitygameparser.game Tcsparitygamelexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	AutomataParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	

let parse_explicit_pg' in_channel init_required =
	let init_value = ref (0) in
	let game = ref [||] in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i pr pl tr de =
		if (i < 0) || (i >= Array.length !game)
		then raise (AutomataParserCustomException ("Node out of range: " ^ (string_of_int i)));
		let (pr', _, _, _) = !game.(i) in
		if pr' >= 0
		then raise (AutomataParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!game.(i) <- (pr, pl, Array.of_list tr, de)
	in
	let add_func_old i pr pl tr de =
		node_list := (i, pr, pl, tr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_func_old _ = (
		game := Array.make (!node_list_max + 1) (-1, -1, [||], "");
		List.iter (fun (a,b,c,d,e) -> add_value a b c d e) !node_list;
		!game
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		game := Array.make (n + 1) (-1, -1, [||], "");
		add_func := add_value;
		ret_func := (fun _ -> !game)
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (AutomataParserCustomException "Init node out of range.")
	)
	in
	let retgame = parse_parity_game has_header has_init (fun a b c d e -> !add_func a b c d e) (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !game 
		then raise (AutomataParserCustomException "Init node out of range.")
		else if (let (pr, _, _, _) = !game.(!init_value) in pr < 0)
		then raise (AutomataParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (AutomataParserCustomException "Initial node statement missing.");
	(!init_value, retgame)

	
let parse_explicit_pg in_channel =
	snd (parse_explicit_pg' in_channel false)
	
let parse_explicit_initpg in_channel =
	parse_explicit_pg' in_channel false

let parse_parity_solution has_header add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__pgsol_has_header := __def_pgsol_has_header;
		__pgsol_add_node := __def_pgsol_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (AutomataParserException (s, l, c))
	);
	__pgsol_has_header := has_header;
	__pgsol_add_node := add_func;
	(try
		Tcsparitysolutionparser.sol Tcsparitysolutionlexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	AutomataParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()


let parse_explicit_parity_solution in_channel =
	let (sol, strat) = (ref [||], ref [||]) in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i pl str =
		if (i < 0) || (i >= Array.length !sol)
		then raise (AutomataParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if !sol.(i) >= 0
		then raise (AutomataParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!sol.(i) <- pl;
		(match str with
			None -> ()
		|	Some j -> !strat.(i) <- j)
	in
	let add_func_old i pl str =
		node_list := (i, pl, str)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_func_old _ = (
		let n = !node_list_max + 1 in
		sol := Array.make n (-1);
		strat := Array.make n (-1);
		List.iter (fun (a,b,c) -> add_value a b c) !node_list;
		(!sol, !strat)
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		sol := Array.make (n+1) (-1);
		strat := Array.make (n+1) (-1);
		add_func := add_value;
		ret_func := (fun _ -> (!sol, !strat))
	)
	in
	parse_parity_solution has_header (fun a b c -> !add_func a b c) (fun a -> !ret_func a) in_channel
	
	
let parse_lts has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__lts_has_header := __def_lts_has_header;
		__lts_has_start := __def_lts_has_start;
		__lts_add_node := __def_lts_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (AutomataParserException (s, l, c))
	);
	__lts_has_header := has_header;
	__lts_has_start := has_start;
	__lts_add_node := add_func;
	(try
		Tcsltsparser.lts Tcsltslexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	AutomataParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	
let parse_explicit_lts' in_channel init_required =
	let init_value = ref (-1) in
	let lts = ref [||] in
	let props = ref StringMap.empty in
	let props_count = ref 0 in
	let labels = ref StringMap.empty in
	let labels_count = ref 0 in
	let register_tr (s,j) =
		try
			(StringMap.find s !labels, j)
		with
			Not_found -> (
				labels := StringMap.add s !labels_count !labels;
				incr labels_count;
				(!labels_count - 1, j)
			)
	in
	let register_pr s =
		try
			StringMap.find s !props
		with
			Not_found -> (
				props := StringMap.add s !props_count !props;
				incr props_count;
				!props_count - 1
			)
	in
	let map_to_array map count = 
		let a = Array.make count "" in
		StringMap.iter (fun s i -> a.(i) <- s) map;
		a
	in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i tr pr de =
		if (i < 0) || (i >= Array.length !lts)
		then raise (AutomataParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if let (_, _, _, v) = !lts.(i) in v
		then raise (AutomataParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!lts.(i) <- (pr, tr, de, true)
	in
	let add_func_old i tr pr de =
		node_list := (i, tr, pr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_lts _ = (map_to_array !props !props_count, map_to_array !labels !labels_count, !lts) in
	let ret_func_old _ = (
		lts := Array.make (!node_list_max + 1) ([||], [||], None, false);
		List.iter (fun (a,b,c,d) -> add_value a b c d) !node_list;
		ret_lts ()
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		lts := Array.make (n + 1) ([||], [||], None, false);
		add_func := add_value;
		ret_func := ret_lts
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (AutomataParserCustomException "Init node out of range.")
	)
	in
	let do_add i tr pr de = !add_func i (Array.of_list (List.map register_tr tr)) (Array.of_list (List.map register_pr pr)) de in
	let retit = parse_lts has_header has_init do_add (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !lts 
		then raise (AutomataParserCustomException "Init node out of range.")
		else if not (let (_, _, _, v) = !lts.(!init_value) in v)
		then raise (AutomataParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (AutomataParserCustomException "Initial node statement missing.");
	(!init_value, retit)

	
let parse_explicit_lts in_channel =
	snd (parse_explicit_lts' in_channel false)
	
let parse_explicit_initlts in_channel =
	parse_explicit_lts' in_channel true


let parse_ts has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__ts_has_header := __def_ts_has_header;
		__ts_has_start := __def_ts_has_start;
		__ts_add_node := __def_ts_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (AutomataParserException (s, l, c))
	);
	__ts_has_header := has_header;
	__ts_has_start := has_start;
	__ts_add_node := add_func;
	(try
		Tcstsparser.ts Tcstslexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	AutomataParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	
let parse_explicit_ts' in_channel init_required =
	let init_value = ref (-1) in
	let ts = ref [||] in
	let props = ref StringMap.empty in
	let props_count = ref 0 in
	let register_pr s =
		try
			StringMap.find s !props
		with
			Not_found -> (
				props := StringMap.add s !props_count !props;
				incr props_count;
				!props_count - 1
			)
	in
	let map_to_array map count = 
		let a = Array.make count "" in
		StringMap.iter (fun s i -> a.(i) <- s) map;
		a
	in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i tr pr de =
		if (i < 0) || (i >= Array.length !ts)
		then raise (AutomataParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if let (_, _, _, v) = !ts.(i) in v
		then raise (AutomataParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!ts.(i) <- (pr, tr, de, true)
	in
	let add_func_old i tr pr de =
		node_list := (i, tr, pr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_ts _ = (map_to_array !props !props_count, !ts) in
	let ret_func_old _ = (
		ts := Array.make (!node_list_max + 1) ([||], [||], None, false);
		List.iter (fun (a,b,c,d) -> add_value a b c d) !node_list;
		ret_ts ()
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		ts := Array.make (n + 1) ([||], [||], None, false);
		add_func := add_value;
		ret_func := ret_ts
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (AutomataParserCustomException "Init node out of range.")
	)
	in
	let do_add i tr pr de = !add_func i (Array.of_list tr) (Array.of_list (List.map register_pr pr)) de in
	let retit = parse_ts has_header has_init do_add (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !ts 
		then raise (AutomataParserCustomException "Init node out of range.")
		else if not (let (_, _, _, v) = !ts.(!init_value) in v)
		then raise (AutomataParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (AutomataParserCustomException "Initial node statement missing.");
	(!init_value, retit)

	
let parse_explicit_ts in_channel =
	snd (parse_explicit_ts' in_channel false)
	
let parse_explicit_initts in_channel =
	parse_explicit_ts' in_channel true
