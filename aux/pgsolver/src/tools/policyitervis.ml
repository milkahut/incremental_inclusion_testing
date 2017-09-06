open Arg ;;
open Tcsargs;;
open Basics ;;
open Paritygame ;;
open Tcsstrings ;;
open Univsolve;;
open Solvers ;;
open Str ;;
open Stratimpralgs ;;

module CommandLine =
struct
  let solver = ref None
  let start_iteration = ref 1
  let end_iteration = ref None
  let add_iteration = ref 0
  (*
  let input_files = ref []
  let timesx = ref 10
  let intelli_timing = ref true
  let silent = ref false
  let gnuplotformat = ref false
  let title = ref "Benchmark Statistics"
  let measure_function = ref get_last_iteration_count
  let innertitle = ref ""
  let timeout = ref (-1.0)
  *)

  let speclist = [
		(["--startiter"; "-si"], Int(fun i -> start_iteration := i),
		 "\n     start with iteration [int] (default: 1)");
		(["--enditer"; "-ei"], Int(fun i -> end_iteration := Some i),
		 "\n     end with iteration [int] (default: infinity)");
		(["--additer"; "-ai"], Int(fun i -> add_iteration := i),
		 "\n     add index to iteration [int] (default: 0)");
  ]
  (*
				[(["--silent"; "-s"], Unit(fun _ -> silent := true),
                      "\n     only output statistics");
                    (["--measureexpbits"; "-meb"], Unit(fun _ -> enable_exp_bit_count := true;
                                                                 measure_function := get_last_exp_bit_count),
                      "\n     measure exponential bits [internal use]");
                    (["--gnuplotformat"; "-gp"], Unit(fun _ -> gnuplotformat := true; silent := true),
                      "\n     output gnuplot line");
                    (["--name"; "-n"], String(fun s -> innertitle := s; title := !title ^ ": " ^ s),
                      "\n     title name of the statistics");
                    (["--times"; "-t"], Int(fun i -> timesx := i; intelli_timing := false),
                      "\n     how many times the solving process is iterated (default is intelligent timing)")] *)

		      @

  fold_solvers (fun solve ident abbrev desc arr ->
  					(["--" ^ ident; "-" ^ abbrev], String(fun solveargs -> let solve = solve (Array.of_list (Tcsstrings.StringUtils.explode solveargs ' ')) in solver := Some solve),
  					 "\n     Use solver: " ^ desc)::arr
  ) []

  let header = Info.get_title "Strategy Improvement Visualizer Tool"
end ;;



let translate (from_ident, from_index_stack) (to_ident, to_index_stack) =
	match (from_ident, to_ident) with
	|	('c', 's') -> Some "edge_lane_t_coord_t_s[0]"
	|	('c', 'r') -> Some "edge_lane_t_coord_t_r[0]"
	|	('t', 'c') -> Some "edge_lane_t_c"
	|	('t', 't') -> Some ("edge_lane_t_t[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('t', 's') -> Some ("edge_lane_t_coord_t_s[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('t', 'r') -> Some ("edge_lane_t_coord_t_r[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('g', 'f') -> Some ("edge_cycle_g_f[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('g', 'k') -> Some ("edge_cycle_g_k[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('e', 'd') -> Some ("edge_cycle_e_b[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('e', 'h') -> Some ("edge_cycle_e_h[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('k', 'z') -> Some ("edge_cycle_k_x[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
	|	('k', 'g') -> Some ("edge_cycle_k_g[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (to_index_stack.(0) + 1) ^ "]")
	|	('r', 'z') -> Some "edge_source_r_x"
	|	('r', 'g') -> Some ("edge_source_r_g[" ^ string_of_int (to_index_stack.(0) + 1) ^ "]")
	|	('s', 'z') -> Some "edge_source_s_x"
	|	('s', 'f') -> Some ("edge_source_s_f[" ^ string_of_int (to_index_stack.(0) + 1) ^ "]")
	|	('u', 'v') -> Some ("edge_cycle_u_v[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('u', 'w') -> Some ("edge_cycle_u_w[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('v', 'm') -> Some ("edge_cycle_v_m[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('v', _) -> Some ("edge_cycle_v_to[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('w', 'q') -> Some ("edge_cycle_w_q[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('w', _) -> Some ("edge_cycle_w_to[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (from_index_stack.(1) + 1) ^ "]")
	|	('d', t) -> let d = if from_index_stack.(1) = 1 then "b" else "d" in
					(
					match t with
						'a' -> Some ("edge_cycle_"^d^"_a[" ^ string_of_int (from_index_stack.(0) + 1) ^ "][" ^ string_of_int (to_index_stack.(0) + 1) ^ "]")
					|	'r' -> Some ("edge_cycle_"^d^"_coord_"^d^"_r[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
					|	's' -> Some ("edge_cycle_"^d^"_coord_"^d^"_s[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
					|	'd' -> Some ("edge_cycle_b_d[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
					|	_ -> Some ("edge_cycle_d_to[" ^ string_of_int (from_index_stack.(0) + 1) ^ "]")
					)
	|	_ -> None

open CommandLine ;;

let out s =
	print_string s;
	flush stdout

type kind = Even_player_strategy | Even_player_disabled | Even_player_improving | Odd_player_strategy | Odd_player_disabled

let _ =
  SimpleArgs.parsedef speclist (fun _ -> ()) (header ^ "Usage: (...)\n" ^ "\nOptions are");
  
(Univsolve.universal_solve_global_options := fun gen_stat verb -> {
	generate_statistics = gen_stat ;
	verb_level = verb ;
	global_optimization = false ;
	decompose_sccs = false ;
	solve_special_games = false ;
	local_optimization = false ;
	globalopt_remove_useless_self_cycles = false ;
	globalopt_solve_useful_self_cycles = false ;
	solvespec_single_parity = false ;
	solvespec_single_player = false ;
	localopt_priority_propagation = false ;
	localopt_compact_priorities = false ;
  });

  let game = parse_parity_game stdin in
  (*
  
	let apply_vars s vars =
		let s = ref s in
		Array.iteri (fun i var ->
			s := Str.global_replace (Str.regexp ("%" ^ string_of_int i)) var !s
		) vars;
	in
	*)

	let before_iteration i =
		out ("beginfig(" ^ string_of_int (i + !add_iteration) ^ ");\n")
	in
	
	let after_iteration i =
		out ("  graph_draw;\n");
		out ("endfig;\n\n")
	in
	
	let set_edge_iteration (from_ident, from_index_stack) (to_ident, to_index_stack) kind =
		match translate (from_ident, from_index_stack) (to_ident, to_index_stack) with
			None -> ()
		|	Some t -> (
				let k =
					match kind with
						Even_player_strategy -> "edge_style_strategy"
					|	Even_player_disabled -> "edge_style_disabled"
					|	Even_player_improving -> "edge_style_improving"
					|	Odd_player_strategy -> "edge_style_counter"
					|	Odd_player_disabled -> "edge_style_discounter"
				in
				out ("  edge_set_style(" ^ t ^ ", " ^ k ^ ");\n")
			)
	in
	
	
	let get_ident i =
		match pg_get_desc game i with
			None -> ('!', [||])
		|	Some s -> 
				if String.length s = 1
				then (String.get s 0, [||])
				else if String.get s 1 = '('
				then (String.get s 0, Array.of_list (List.map int_of_string (StringUtils.explode (List.hd (StringUtils.explode (StringUtils.rest_string s 2) ')')) ',')))
				else (String.get s 0, [|int_of_string (StringUtils.rest_string s 1)|])
	in
	
	
	_strat_impr_callback := Some (fun strat counter ->
		let node_compare = node_total_ordering_by_position in
		let valu = evaluate_strategy game node_compare strat in
		let less i j = node_valuation_ordering game node_compare valu.(i) valu.(j) < 0 in
		let counter_strat =
			Array.init (Array.length valu) (fun i ->
				if pg_get_pl game i = 1
				then best_decision_by_valuation_ordering game node_compare valu i
				else -1
			)
		in

		if counter >= !start_iteration && (match !end_iteration with None -> true | Some end_idx -> counter <= end_idx) then (
			before_iteration counter;
    		Array.iteri (fun i (_, pl, tr, _) ->
				let from_node = get_ident i in
                Array.iter (fun j ->
					let to_node = get_ident j in
					let kind =
						if pl = 0 then
							if strat.(i) = j
                            then Even_player_strategy
                            else if less j strat.(i)
                            then Even_player_disabled
                            else Even_player_improving
                        else
                        	if counter_strat.(i) = j
                        	then Odd_player_strategy
							else Odd_player_disabled
                    in
                        set_edge_iteration from_node to_node kind
                ) tr
    		) game;			
			after_iteration counter;
		);
	);
	
	match !solver with
		None -> ()
	|	Some solve -> let _ = solve game in ()
	

	