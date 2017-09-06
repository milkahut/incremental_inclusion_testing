open Basics;;
open Tcsset;;
open Tcsarray;;
open Tcstiming;;
open Tcsqueue;;
open Tcsstrings;;
open Paritygame;;
open Transformations;;
open Specialsolve;;


(* TODO: - Add transitive hull / remove edge stuff *)




(******************************************************************************
 *                                                                            *
 * Universal Solver Options                                                   *
 *                                                                            *
 ******************************************************************************)

type universal_solve_options = {
	(* General configuration *)
	generate_statistics: bool; (* default: false *)
	verb_level: verbosity_level; (* default: 3 *)

	decompose_sccs: bool; (* default: true *)

	(* Optimization phases *)
	global_optimization: bool; (* default: true *)
	solve_special_games: bool; (* default: true *)
	local_optimization: bool; (* default: true *)

	(* Global optimization *)
	globalopt_remove_useless_self_cycles: bool; (* default: true *)
	globalopt_solve_useful_self_cycles: bool; (* default: true *)

	(* Solving special games *)
	solvespec_single_parity: bool; (* default: true *)
	solvespec_single_player: bool; (* default: true *)

	(* Local optimization *)
	localopt_priority_propagation: bool; (* default: false *)
	localopt_compact_priorities: bool; (* default: true *)
};;

let universal_solve_def_options gen_statistics verbos_level = {
	(* General configuration *)
	generate_statistics = gen_statistics;
	verb_level = verbos_level;

	decompose_sccs = true;

	(* Optimization phases *)
	global_optimization = true;
	solve_special_games = true;
	local_optimization = true;

	(* Global optimization *)
	globalopt_remove_useless_self_cycles = true;
	globalopt_solve_useful_self_cycles = true;

	(* Solving special games *)
	solvespec_single_parity = true;
	solvespec_single_player = true;

	(* Local optimization *)
	localopt_priority_propagation = false;
	localopt_compact_priorities = true;
};;

let universal_solve_global_options = ref universal_solve_def_options;;

let universal_solve_init_options_verbose f = f (verbosity_level_verbose <= (min !Basics.verbosity 2)) verbosity_level_verbose

let universal_options_alter_verb options verb =
{
	generate_statistics = options.generate_statistics;
	verb_level = verb;
	decompose_sccs = options.decompose_sccs;
	global_optimization = options.global_optimization;
	solve_special_games = options.solve_special_games;
	local_optimization = options.local_optimization;
	globalopt_remove_useless_self_cycles = options.globalopt_remove_useless_self_cycles;
	globalopt_solve_useful_self_cycles = options.globalopt_solve_useful_self_cycles;
	solvespec_single_parity = options.solvespec_single_parity;
	solvespec_single_player = options.solvespec_single_player;
	localopt_priority_propagation = options.localopt_priority_propagation;
	localopt_compact_priorities = options.localopt_compact_priorities;
};;


(******************************************************************************
 *                                                                            *
 * Universal Solver Statistics                                                *
 *                                                                            *
 ******************************************************************************)

type universal_solve_statistics = {
	(* General statistics *)
	overall_timing: SimpleTiming.timing_object;
	universal_timing: SimpleTiming.timing_object;
	backend_timing: SimpleTiming.timing_object;
	logistics_timing: SimpleTiming.timing_object;
	stats_timing: SimpleTiming.timing_object;
	overall_solved_nodes: int ref; (* size of game *)
	universal_solved_nodes: int ref;
	backend_solved_nodes: int ref;
	backend_investigated_nodes: int ref;
	index_sum: int ref;
	index_reduced_sum: int ref;
	index_count: int ref;

	(* Optimization phase timing *)
	global_timing: SimpleTiming.timing_object;
	global_timing_without_attractor: SimpleTiming.timing_object;
	global_nodes: int ref;
	global_nodes_without_attractor: int ref;
	special_timing: SimpleTiming.timing_object;
	special_nodes: int ref;
	local_timing: SimpleTiming.timing_object;

	(* Recursion and decomposition *)
	decomposition_timing: SimpleTiming.timing_object;
	max_recursion_depth: int ref;
	recursive_calls: int ref;
	toplevel_sccs: int ref;
	largest_toplevel_scc: int ref;
	total_sccs: int ref;

	(* Attractor closure *)
	attractor_timing: SimpleTiming.timing_object;
	attractor_investigated_nodes: int ref;
	attractor_solved_nodes: int ref;

	(* Global optimization *)
	globalopt_remove_useless_self_cycles_nodes: int ref;
	globalopt_remove_useless_self_cycles_timing: SimpleTiming.timing_object;
	globalopt_solve_useful_self_cycles_nodes: int ref;
	globalopt_solve_useful_self_cycles_timing: SimpleTiming.timing_object;

	(* Special games *)
	solvespec_single_parity_nodes: int ref;
	solvespec_single_parity_timing: SimpleTiming.timing_object;
	solvespec_single_player_nodes: int ref;
	solvespec_single_player_timing: SimpleTiming.timing_object;

	(* Local optimization *)
	localopt_priority_propagation_timing: SimpleTiming.timing_object;
	localopt_compact_priorities_timing: SimpleTiming.timing_object;
};;


type alignment = L of string | R of string;;

let universal_solve_format_stats stats =
	let doubleline = "+=============================================================================+\n" in
	let singleline = "+-----------------------------------------------------------------------------+\n" in
	let topline = "| Universal Solver Statistics                                                 |\n" in
	let fmtal len = function L s -> StringUtils.fillup s len ' ' | R s -> StringUtils.fillup_left s len ' ' in
	let infoline ident info1 info2 info3 =
		"| " ^ (fmtal (String.length "                              ") ident) ^ " | " ^ (fmtal (String.length "            ") info1) ^ " | " ^ (fmtal (String.length "            ") info2)  ^ " | " ^ (fmtal (String.length "            ") info3) ^ " |\n"
	in
	doubleline ^ topline ^ doubleline ^
	infoline (L "Solving modules") (R "Solved") (R "Investigated") (R "CPU-time") ^ singleline ^
	infoline (L "Overall") (R (string_of_int !(stats.overall_solved_nodes))) (R "-") (R (SimpleTiming.format stats.overall_timing)) ^
	infoline (L "Backend") (R (string_of_int !(stats.backend_solved_nodes))) (R (string_of_int !(stats.backend_investigated_nodes))) (R (SimpleTiming.format stats.backend_timing)) ^
	infoline (L "Universal") (R (string_of_int !(stats.universal_solved_nodes))) (R "-") (R (SimpleTiming.format stats.universal_timing)) ^
	singleline ^
	infoline (L "Attractor") (R (string_of_int !(stats.attractor_solved_nodes))) (R (string_of_int !(stats.attractor_investigated_nodes))) (R (SimpleTiming.format stats.attractor_timing)) ^
	infoline (L "Global optimization") (R (string_of_int !(stats.global_nodes))) (R "-") (R (SimpleTiming.format stats.global_timing)) ^
	infoline (L "Global optimization wo. attr.") (R (string_of_int !(stats.global_nodes_without_attractor))) (R "-") (R (SimpleTiming.format stats.global_timing_without_attractor)) ^
	infoline (L "Useful self cycles") (R (string_of_int !(stats.globalopt_solve_useful_self_cycles_nodes))) (R "-") (R (SimpleTiming.format stats.globalopt_solve_useful_self_cycles_timing)) ^
	infoline (L "Special games") (R (string_of_int !(stats.special_nodes))) (R "-") (R (SimpleTiming.format stats.special_timing)) ^
	infoline (L "Single parity games") (R (string_of_int !(stats.solvespec_single_parity_nodes))) (R "-") (R (SimpleTiming.format stats.solvespec_single_parity_timing)) ^
	infoline (L "Single player games") (R (string_of_int !(stats.solvespec_single_player_nodes))) (R "-") (R (SimpleTiming.format stats.solvespec_single_player_timing)) ^
	doubleline ^
	infoline (L "Decomposition") (R "Rec. Depth") (R "Rec. Calls") (R "CPU-time") ^
	infoline (L "") (R (string_of_int !(stats.max_recursion_depth))) (R (string_of_int !(stats.recursive_calls))) (R (SimpleTiming.format stats.decomposition_timing)) ^
	doubleline ^
	infoline (L "Strongly Connected Components") (R "Toplevel") (R "Total") (R "Largest") ^
	infoline (L "") (R (string_of_int !(stats.toplevel_sccs))) (R (string_of_int !(stats.total_sccs))) (R (string_of_int !(stats.largest_toplevel_scc))) ^
	(if !(stats.toplevel_sccs) = 0 then "" else
	infoline (L "") (R "") (R "") (R "Mean Size") ^
	infoline (L "") (R "") (R "") (R (string_of_int (!(stats.overall_solved_nodes) / !(stats.toplevel_sccs)))) )^ 
	doubleline ^
	infoline (L "Useless Self Cycles") (R "Removed") (R "") (R "CPU-time") ^
	infoline (L "") (R (string_of_int !(stats.globalopt_remove_useless_self_cycles_nodes))) (R "") (R (SimpleTiming.format stats.globalopt_remove_useless_self_cycles_timing)) ^
	doubleline ^
	infoline (L "Local Optimization") (R "Index mean") (R "Reduced mean") (R "CPU-time") ^
	infoline (L "") (if !(stats.index_count) > 0 then (R (string_of_int (!(stats.index_sum) / !(stats.index_count)))) else (R "-")) (if !(stats.index_count) > 0 then (R (string_of_int (!(stats.index_reduced_sum) / !(stats.index_count)))) else (R "-")) (R (SimpleTiming.format stats.local_timing)) ^
	doubleline ^
	infoline (L "Priority propagation timing") (R "") (R "") (R (SimpleTiming.format stats.localopt_priority_propagation_timing)) ^
	infoline (L "Priority compression timing") (R "") (R "") (R (SimpleTiming.format stats.localopt_compact_priorities_timing)) ^
	doubleline ^
	infoline (L "Logistics timing") (R "") (R "") (R (SimpleTiming.format stats.logistics_timing)) ^
	doubleline ^
	infoline (L "Statistics timing") (R "") (R "") (R (SimpleTiming.format stats.stats_timing)) ^
	doubleline ^ "\n";;

let universal_solve_init_statistics _ = {
    (* General statistics *)
    overall_timing = SimpleTiming.init false;
    universal_timing = SimpleTiming.init false;
    backend_timing = SimpleTiming.init false;
    logistics_timing = SimpleTiming.init false;
    stats_timing = SimpleTiming.init false;
    overall_solved_nodes = ref 0;
    universal_solved_nodes = ref 0;
    backend_solved_nodes = ref 0;
    backend_investigated_nodes = ref 0;
    index_sum = ref 0;
    index_reduced_sum = ref 0;
    index_count = ref 0;

    (* Optimization phase timing *)
    global_timing = SimpleTiming.init false;
    global_timing_without_attractor = SimpleTiming.init false;
    global_nodes = ref 0;
    global_nodes_without_attractor = ref 0;
    special_timing = SimpleTiming.init false;
    special_nodes = ref 0;
    local_timing = SimpleTiming.init false;

    (* Recursion and decomposition *)
    decomposition_timing = SimpleTiming.init false;
    max_recursion_depth = ref 0;
    recursive_calls = ref 0;
    toplevel_sccs = ref 0;
    largest_toplevel_scc = ref 0;
    total_sccs = ref 0;

    (* Attractor closure *)
    attractor_timing = SimpleTiming.init false;
    attractor_investigated_nodes = ref 0;
    attractor_solved_nodes = ref 0;

    (* Global optimization *)
    globalopt_remove_useless_self_cycles_nodes = ref 0;
    globalopt_remove_useless_self_cycles_timing = SimpleTiming.init false;
    globalopt_solve_useful_self_cycles_nodes = ref 0;
    globalopt_solve_useful_self_cycles_timing = SimpleTiming.init false;

    (* Special games *)
    solvespec_single_parity_nodes = ref 0;
    solvespec_single_parity_timing = SimpleTiming.init false;
    solvespec_single_player_nodes = ref 0;
    solvespec_single_player_timing = SimpleTiming.init false;

    (* Local optimization *)
    localopt_priority_propagation_timing = SimpleTiming.init false;
    localopt_compact_priorities_timing = SimpleTiming.init false;
};;








type universal_msg_tag = DEFAULT | GLOBAL | SPECIAL | LOCAL | DECOMP | MAIN | BACKEND | ATTRACTOR;;

let format_universal_msg_tag = function DEFAULT -> "DEFAULT" | GLOBAL -> "GLOBAL" | SPECIAL -> "SPECIAL" | LOCAL -> "LOCAL" | DECOMP -> "DECOMP" | MAIN -> "MAIN" | BACKEND -> "BACKEND" | ATTRACTOR -> "ATTRACTOR";;

let universal_solve_run options stats backend game' =

	let active_timers = ref [] in

	(* Timer helper *)
	let timer_start tim = if options.generate_statistics then (
							active_timers := tim::!active_timers;
							SimpleTiming.start tim
						  ) in
	let timer_stop tim = if options.generate_statistics then (
							active_timers := List.filter (fun t -> not (t == tim)) !active_timers;
							SimpleTiming.stop tim
					     ) in

	let start_stat _ = (
		if options.generate_statistics then (
            SimpleTiming.start stats.stats_timing;
            List.iter SimpleTiming.stop !active_timers
        )
	) in
	let stop_stat _ = (
		if options.generate_statistics then (
			SimpleTiming.stop stats.stats_timing;
			List.iter SimpleTiming.start !active_timers
		)
	) in

	(* Stats helper *)
	let stat_addint l f =
		if options.generate_statistics then (
			start_stat ();
			let res = f () in
			List.iter (fun intref -> intref := !intref + res) l;
			stop_stat ()
		)
	in

	(* Message *)
	let msg_plain tag verbosity f =
		start_stat ();
		message (options.verb_level + verbosity) f;
		stop_stat ()
	in

	let msg_tagged tag verbosity f =
		start_stat ();
		message_autotagged (options.verb_level + verbosity) (fun _ -> format_universal_msg_tag tag) f;
		stop_stat ()
	in

	let msg_incrdepth u = message_incrdepth u in

	let msg_decrdepth u = message_decrdepth u in


	(************************************************************
	 * DECOMPOSITION                                            *
	 ************************************************************)

	let rec universal_solve_decompose game transp recdepth =
        let n = pg_size game in

		msg_incrdepth ();
        msg_tagged DECOMP 0 (fun _ -> "Entering decomposition phase at recursion level " ^ string_of_int recdepth ^ "\n");
        msg_tagged DECOMP 0 (fun _ -> "Considering game of size " ^ string_of_int (pg_node_count game) ^ "\n");

        stats.max_recursion_depth := max recdepth !(stats.max_recursion_depth);
        stats.recursive_calls := !(stats.recursive_calls) + 1;

		timer_start stats.logistics_timing;
        let sol = Array.make n (-1) in
        let strat = Array.make n (-1) in
        timer_stop stats.logistics_timing;

        timer_start stats.decomposition_timing;

	let dummy_decomposition game =
		let n = pg_size game in
		let l = ref [] in
		let a = Array.make n (-1) in
		for i = n - 1 downto 0 do
			if pg_get_pr game i >= 0 then (
				l := i::!l;
				a.(i) <- 0;
			)
		done;
		([|!l|], a, [|[]|], [0])
	in

	let strongly_connected_components' game =
        let rec deflist l n = if n = 0 then 0::l else deflist (n::l) (n - 1) in
		if options.decompose_sccs then (
            let (sccs, sccindex, topology, roots) = strongly_connected_components game in
            if Array.length sccs = 1
            then dummy_decomposition game
            else (sccs, sccindex, topology, roots)
        )
        else if pg_node_count game > 0
        then dummy_decomposition game
        else ([|[]|], [||], [||], [])
	in

        let (sccs, sccindex, topology, roots) = strongly_connected_components' game in
        let sccs_count = Array.length sccs in
        let solvedscc = Array.make sccs_count false in
        let touchedscc = Array.make sccs_count false in

        if recdepth = 0 then stat_addint [stats.toplevel_sccs] (fun _ -> Array.length sccs);
        stat_addint [stats.total_sccs] (fun _ -> Array.length sccs);
        if (Array.length sccs > 0) && (recdepth = 0)
        then stat_addint [stats.largest_toplevel_scc] (fun _ -> List.length (ArrayUtils.max_elt (fun l1 l2 -> compare (List.length l1) (List.length l2)) sccs));
        msg_tagged DECOMP 0 (fun _ -> "Decomposed game into " ^ string_of_int (Array.length sccs) ^ " SCCs");
        msg_plain DECOMP 0 (fun _ -> if Array.length sccs > 0 then ", size of largest SCC is " ^ string_of_int (List.length (ArrayUtils.max_elt (fun l1 l2 -> compare (List.length l1) (List.length l2)) sccs)) ^ "\n" else "\n");
        timer_stop stats.decomposition_timing;

        let subgame_solve nodes solver =
           	timer_start stats.logistics_timing;
        	let solved = ref [] in
            let sg = subgame_by_list game nodes in
           	timer_stop stats.logistics_timing;
            let (sol', strat') = solver sg in
           	timer_start stats.logistics_timing;
            let n = List.length nodes in
            let idxmap = Array.make n (-1) in
            let i = ref 0 in
            List.iter (fun q ->
                idxmap.(!i) <- q;
                i := !i + 1
            ) nodes;
            for j = 0 to n - 1 do
                sol.(idxmap.(j)) <- sol'.(j);
                if sol'.(j) >= 0
                then solved := idxmap.(j)::!solved
                else failwith "subgame was not solved!";
                if (strat'.(j) >= 0) then strat.(idxmap.(j)) <- idxmap.(strat'.(j))
            done;
           	timer_stop stats.logistics_timing;
            !solved
        in

        let solve_scc game recdepth =
        	let result = ref None in

            (************************************************************
             * SPECIAL GAMES                                            *
             ************************************************************)

        	if options.solve_special_games && options.decompose_sccs then (
				timer_start stats.special_timing;
        		if (!result = None) && options.solvespec_single_parity then (
        			timer_start stats.solvespec_single_parity_timing;
        			(
                        match (is_single_parity_game game) with None -> () |
                            Some player -> (
			        			msg_tagged SPECIAL 0 (fun _ -> "Solving this single parity SCC...");
                                result := Some (solve_single_parity_scc game player);
                                stat_addint [stats.solvespec_single_parity_nodes;
                                             stats.special_nodes;
                                             stats.universal_solved_nodes;
                                             stats.overall_solved_nodes] (fun _ -> pg_size game);
			        			msg_plain SPECIAL 0 (fun _ -> "done!\n")
                            )
                    );
        			timer_stop stats.solvespec_single_parity_timing;
        		);
        		if (!result = None) && options.solvespec_single_player then (
        			timer_start stats.solvespec_single_player_timing;
        			(
                        match (get_player_decision_info game) with (true, true) -> () |
                            (false, false) -> (
                            	msg_tagged SPECIAL 0 (fun _ -> "Solving this cycle SCC...");
                                result := Some (solve_cycle_scc game);
                                stat_addint [stats.solvespec_single_player_nodes;
                                             stats.special_nodes;
                                             stats.universal_solved_nodes;
                                             stats.overall_solved_nodes] (fun _ -> pg_size game);
			        			msg_plain SPECIAL 0 (fun _ -> "done!\n")
                            )
                        |   (pl0, _) -> (
                                let pl = if pl0 then 0 else 1 in
                            	msg_tagged SPECIAL 0 (fun _ -> "Solving single player (" ^ string_of_int pl ^ ") SCC...");
                                result := Some (solve_single_player_scc game pl);
                                stat_addint [stats.solvespec_single_player_nodes;
                                             stats.special_nodes;
                                             stats.universal_solved_nodes;
                                             stats.overall_solved_nodes] (fun _ -> pg_size game);
			        			msg_plain SPECIAL 0 (fun _ -> "done!\n")
                            )
                    );
        			timer_stop stats.solvespec_single_player_timing;
        		);
				timer_stop stats.special_timing;
        	);

			if !result = None then (

                (************************************************************
                 * LOCAL OPTIMIZATION                                       *
                 ************************************************************)

				if options.local_optimization then (
					timer_start stats.local_timing;
				   	stat_addint [stats.index_sum] (fun _ -> pg_get_index game);
				   	msg_tagged LOCAL 0 (fun _ -> "Reducing index " ^ string_of_int (pg_get_index game) ^ " down to... ");
					if options.localopt_priority_propagation then (
						timer_start stats.localopt_priority_propagation_timing;
						priority_propagation_inplace game;
						timer_stop stats.localopt_priority_propagation_timing
					);
					if options.localopt_compact_priorities then (
						timer_start stats.localopt_compact_priorities_timing;
						let _ = compact_prio_inplace game true in
						timer_stop stats.localopt_compact_priorities_timing;
					);
				   	stat_addint [stats.index_reduced_sum] (fun _ -> pg_get_index game);
				   	stat_addint [stats.index_count] (fun _ -> 1);
				   	msg_plain LOCAL 0 (fun _ -> string_of_int (pg_get_index game) ^ "!\n");
					timer_stop stats.local_timing;
				);

                (************************************************************
                 * BACKEND                                                  *
                 ************************************************************)

				timer_stop stats.universal_timing;
				timer_start stats.backend_timing;
				msg_incrdepth ();
				msg_tagged BACKEND 0 (fun _ -> "Calling backend for this SCC...\n");
				let (sol, strat) = backend game in
				msg_tagged BACKEND 0 (fun _ -> "Backend returned and solved ");
				msg_decrdepth ();
				timer_stop stats.backend_timing;
				timer_start stats.universal_timing;
				timer_start stats.logistics_timing;
				let n = pg_size game in
                let (w0, w1) = (ref IntSet.empty, ref IntSet.empty) in
                let counter = ref 0 in
                for i = 0 to (Array.length game) - 1 do
                    if (sol.(i) = 0) then w0 := IntSet.add i !w0;
                    if (sol.(i) = 1) then w1 := IntSet.add i !w1;
                    if sol.(i) >= 0 then incr counter;
                    let (pr, pl, _, _) = game.(i) in
                    if (pr < 0) || (pl != sol.(i))
                    then strat.(i) <- -1;
                done;
				msg_plain BACKEND 0 (fun _ -> string_of_int !counter ^ " out of " ^ string_of_int n ^ "\n");

                stat_addint [stats.backend_investigated_nodes] (fun _ -> n);
                stat_addint [stats.backend_solved_nodes;
                			 stats.overall_solved_nodes] (fun _ -> !counter);
				timer_stop stats.logistics_timing;
                if !counter = 0
                then failwith "universal_solve fatal error: backend solved nothing at all!";
                if !counter < n then (
                	timer_start stats.logistics_timing;
                	let transp = game_to_transposed_graph game in
                	timer_stop stats.logistics_timing;
                	timer_start stats.attractor_timing;
                	msg_tagged ATTRACTOR 0 (fun _ -> "Building attractor... ");
                	let (sol0, sol1) = attractor_closure_inplace_sol_strat game transp (fun _ -> true) sol strat !w0 !w1 in

                	msg_plain ATTRACTOR 0 (fun _ -> "investigated " ^ (string_of_int (List.length sol0 + List.length sol1)) ^ ", adding "  ^ (string_of_int (IntSet.cardinal (IntSet.union (IntSetUtils.of_list sol0) (IntSetUtils.of_list sol1)) - !counter)) ^ "!\n");
                    stat_addint [stats.attractor_investigated_nodes] (fun _ -> List.length sol0 + List.length sol1);

                    stat_addint [stats.attractor_solved_nodes;
                                 stats.overall_solved_nodes;
                                 stats.universal_solved_nodes]
                                 (fun _ -> IntSet.cardinal (IntSet.union (IntSetUtils.of_list sol0) (IntSetUtils.of_list sol1)) - !counter);
                	timer_stop stats.attractor_timing;

                	if (List.length sol0) + (List.length sol1) < n then (
   						msg_tagged BACKEND 0 (fun _ -> "SCC was not completely solved.\n");

                		timer_start stats.logistics_timing;
						pg_with_graph_remove_nodes game transp sol0;
						pg_with_graph_remove_nodes game transp sol1;
                        timer_stop stats.logistics_timing;
                		let (sol', strat') = universal_solve_decompose game transp (recdepth + 1) in
                		timer_start stats.logistics_timing;
                        merge_strategies_inplace strat strat';
                        merge_solutions_inplace sol sol';
                        timer_stop stats.logistics_timing
                	)
                );
                result := Some (sol, strat)
			);
			match !result with Some res -> res | None -> failwith "impossible"
        in

        let attractor_closure init =
        	let q = SingleOccQueue.create () in
        	let attr = ref [] in
        	List.iter (fun i -> SingleOccQueue.add i q) init;
        	while (not (SingleOccQueue.is_empty q)) do
        		let v = SingleOccQueue.take q in
        		let winner = sol.(v) in
        		let to_remove = ref [] in
        		List.iter (fun w ->
        			if sol.(w) < 0 then (
        				let (pr, pl, delta, ann) = game.(w) in
        				if (pl = winner) then (
        					sol.(w) <- winner;
        					touchedscc.(sccindex.(w)) <- true;
        					strat.(w) <- v;
        					SingleOccQueue.add w q;
        					attr := w::!attr
        				)
        				else if Array.length delta = 1 then (
        					sol.(w) <- winner;
        					touchedscc.(sccindex.(w)) <- true;
        					SingleOccQueue.add w q;
        					attr := w::!attr
        				)
        				else to_remove := (w, v)::!to_remove
        			)
        		) transp.(v);
				pg_remove_edges game !to_remove;
				transposed_graph_remove_edges transp !to_remove
        	done;
        	!attr
        in

        (************************************************************
         * RECURSIVE PROCESSING OF SCCs                             *
         ************************************************************)

        let rec process_scc r =
        	if not solvedscc.(r) then (
        		List.iter process_scc topology.(r);
                solvedscc.(r) <- true;
                timer_start stats.logistics_timing;
        		let untouched = if (touchedscc.(r))
        						then Some (List.filter (fun v -> sol.(v) < 0) sccs.(r))
        						else None
        		in
        		timer_stop stats.logistics_timing;
        		if not (untouched = Some []) then (
                    let attrsrc =
                        match untouched with
                            Some l ->
                                let f g =
                                    timer_start stats.logistics_timing;
                                    let tr = game_to_transposed_graph g in
                                    timer_stop stats.logistics_timing;
                                    universal_solve_decompose g tr (recdepth + 1)
                                in (
                                	msg_tagged DECOMP 0 (fun _ -> "Entering SCC #" ^ string_of_int r ^ " of size " ^ string_of_int (List.length sccs.(r)) ^ " with " ^ string_of_int (List.length sccs.(r) - List.length l) ^ " solved and " ^ string_of_int (List.length l) ^ " unsolved nodes\n");
		                             let res = subgame_solve l f in
		                             res
		                        )
                        |   None -> if (List.length sccs.(r) > 1)
                        			then (
                        				msg_tagged DECOMP 0 (fun _ -> "Entering SCC #" ^ string_of_int r ^ " of size " ^ string_of_int (List.length sccs.(r)) ^ "\n");
                        				subgame_solve sccs.(r) (fun g -> solve_scc g recdepth)
                        			)
                        			else (
										stat_addint [stats.universal_solved_nodes;
													 stats.overall_solved_nodes] (fun _ -> 1);
                                        let h = List.hd sccs.(r) in
                                        let (pr, pl, d, _) = game.(h) in
                                        if (Array.length d = 0)
                                        then sol.(h) <- 1 - pl
                                        else (
                                            sol.(h) <- pr mod 2;
                                            if (sol.(h) = pl)
                                            then strat.(h) <- h
					                    );
					                	[h]
					                )
					in
						timer_start stats.attractor_timing;
	                	msg_tagged ATTRACTOR 0 (fun _ -> "Building attractor... ");
						let attr = attractor_closure attrsrc in
                		msg_plain ATTRACTOR 0 (fun _ -> "investigated " ^ (string_of_int (List.length attrsrc + List.length attr)) ^ ", adding "  ^ (string_of_int (List.length attr)) ^ "!\n");

	                    stat_addint [stats.attractor_investigated_nodes] (fun _ -> List.length attrsrc + List.length attr);

	                    stat_addint [stats.attractor_solved_nodes;
                                 	 stats.overall_solved_nodes;
                                 	 stats.universal_solved_nodes]
                                 	(fun _ -> List.length attr);

                        timer_stop stats.attractor_timing;

                )
        	)
        in

        List.iter process_scc roots;
		msg_tagged DECOMP 0 (fun _ -> "Leaving decomposition phase at recursion level " ^ string_of_int recdepth ^ "\n");
		msg_decrdepth ();

        (sol, strat)

	in

	(************************************************************
	 * START OF UNIVERSAL SOLVER                                *
	 ************************************************************)

	timer_start stats.overall_timing;
	timer_start stats.universal_timing;
	msg_plain MAIN 0 (fun _ -> "\n");
	msg_tagged MAIN 0 (fun _ -> "Starting Universal Solving Process\n");

	timer_start stats.logistics_timing;

	let n = Array.length game' in
	let m = pg_node_count game' in
	let solution = Array.make n (-1) in
	let strategy = Array.make n (-1) in

	timer_stop stats.logistics_timing;

	msg_tagged MAIN 0 (fun _ -> "Considering game...\n");
	msg_tagged MAIN 0 (fun _ -> "  Nodes = " ^ string_of_int m ^ "\n");
	msg_tagged MAIN 0 (fun _ -> "  Edges = " ^ string_of_int (pg_edge_count game') ^ "\n");
	msg_tagged MAIN 0 (fun _ -> "  Index = " ^ string_of_int (pg_get_index game') ^ "\n");
	
	if m > 0 then (

        timer_start stats.logistics_timing;
		msg_tagged MAIN 0 (fun _ -> "Building transposed graph...");
        let game = pg_copy game' in
        let transp = game_to_transposed_graph game in
		msg_plain MAIN 0 (fun _ -> "finished\n");

        timer_stop stats.logistics_timing;



        (************************************************************
         * GLOBAL OPTIMIZATION                                      *
         ************************************************************)

        if options.global_optimization then (
            timer_start stats.global_timing;
            timer_start stats.global_timing_without_attractor;

            msg_tagged GLOBAL 0 (fun _ -> "Entering global preprocessing phase...\n");

            if options.globalopt_remove_useless_self_cycles then (
                timer_start stats.globalopt_remove_useless_self_cycles_timing;

                msg_tagged GLOBAL 0 (fun _ -> "Removing useless self cycles... ");
                let removed = remove_useless_self_cycles_inplace game in
                transposed_graph_remove_edges transp (List.map (fun i -> (i,i)) removed);
                let removedl = List.length removed in
                stat_addint [stats.globalopt_remove_useless_self_cycles_nodes] (fun _ -> removedl);
                msg_plain GLOBAL 0 (fun _ -> string_of_int removedl ^ " transition(s) removed!\n");
                
                let sinks = List.filter (fun i -> Array.length (pg_get_tr game i) = 0) removed in
                let sinksl = List.length sinks in
                
                msg_tagged GLOBAL 0 (fun _ -> "Created sinks: " ^ string_of_int sinksl ^ "!\n");

                timer_stop stats.globalopt_remove_useless_self_cycles_timing;

                if sinksl > 0 then (
                    let (w0, w1) = (ref IntSet.empty, ref IntSet.empty) in
                    List.iter (fun i ->
                    	let player = pg_get_pl game i in
                        let w = if player = 0 then w1 else w0 in
                        w := IntSet.add i !w;
                        solution.(i) <- 1 - player;
                    ) sinks;

                    timer_stop stats.global_timing_without_attractor;
                    timer_start stats.attractor_timing;
                    
                	msg_tagged ATTRACTOR 0 (fun _ -> "Building attractor... ");
                    let (sol0, sol1) = attractor_closure_inplace_sol_strat game transp (fun _ -> true)
                                                                           solution strategy !w0 !w1 in
                                                                           
                    let solcount = List.length sol0 + List.length sol1 in
               		msg_plain ATTRACTOR 0 (fun _ -> (string_of_int solcount) ^ " nodes added!\n");

                    stat_addint [stats.attractor_investigated_nodes] (fun _ -> solcount);

                    stat_addint [stats.global_nodes;
                                 stats.attractor_solved_nodes;
                                 stats.overall_solved_nodes;
                                 stats.universal_solved_nodes]
                                 (fun _ -> List.length sol0 + List.length sol1);

                    timer_stop stats.attractor_timing;
                    timer_start stats.global_timing_without_attractor;

                    timer_start stats.logistics_timing;
					pg_with_graph_remove_nodes game transp sol0;
					pg_with_graph_remove_nodes game transp sol1;
                    timer_stop stats.logistics_timing;
                );

            );

            if options.globalopt_solve_useful_self_cycles then (
                timer_start stats.globalopt_solve_useful_self_cycles_timing;

                msg_tagged GLOBAL 0 (fun _ -> "Searching for useful self cycles... ");
                let selfcycles = find_useful_self_cycles game in
                msg_plain GLOBAL 0 (fun _ -> string_of_int (List.length selfcycles) ^ " node(s) found!\n");

                stat_addint [stats.globalopt_solve_useful_self_cycles_nodes;
                             stats.global_nodes;
                             stats.global_nodes_without_attractor;
                             stats.overall_solved_nodes;
                             stats.universal_solved_nodes]
                             (fun _ -> List.length selfcycles);

                timer_stop stats.globalopt_solve_useful_self_cycles_timing;

                if not (selfcycles = []) then (
                    timer_start stats.logistics_timing;
                    let nodes = ref [] in
                    let (w0, w1) = (ref IntSet.empty, ref IntSet.empty) in
                    List.iter (fun (node, player, move) ->
                        nodes := node::!nodes;
                        solution.(node) <- player;
                        strategy.(node) <- move;
                        let w = if player = 0 then w0 else w1 in
                        w := IntSet.add node !w
                    ) selfcycles;
                    timer_stop stats.logistics_timing;

                    timer_stop stats.global_timing_without_attractor;
                    timer_start stats.attractor_timing;
                	msg_tagged ATTRACTOR 0 (fun _ -> "Building attractor... ");
                    let (sol0, sol1) = attractor_closure_inplace_sol_strat game transp (fun _ -> true)
                                                                           solution strategy !w0 !w1 in
               		msg_plain ATTRACTOR 0 (fun _ -> "investigated " ^ (string_of_int (List.length sol0 + List.length sol1)) ^ ", removing "  ^ (string_of_int (IntSet.cardinal (IntSet.union (IntSetUtils.of_list sol0) (IntSetUtils.of_list sol1)))) ^ "!\n");

                    stat_addint [stats.attractor_investigated_nodes] (fun _ -> List.length sol0 + List.length sol1);

                    stat_addint [stats.global_nodes;
                                 stats.attractor_solved_nodes;
                                 stats.overall_solved_nodes;
                                 stats.universal_solved_nodes]
                                 (fun _ -> IntSet.cardinal (IntSet.union (IntSetUtils.of_list sol0) (IntSetUtils.of_list sol1)) - (List.length selfcycles));

                    timer_stop stats.attractor_timing;
                    timer_start stats.global_timing_without_attractor;

                    timer_start stats.logistics_timing;
					pg_with_graph_remove_nodes game transp sol0;
					pg_with_graph_remove_nodes game transp sol1;
                    timer_stop stats.logistics_timing;

                );

            );

            timer_stop stats.global_timing_without_attractor;
            timer_stop stats.global_timing
        );


        (************************************************************
         * DECOMPOSITION CALL                                       *
         ************************************************************)
         
        let (sol, strat) = universal_solve_decompose game transp 0 in

        timer_start stats.logistics_timing;
        merge_strategies_inplace strategy strat;
        merge_solutions_inplace solution sol;
        timer_stop stats.logistics_timing;
	);

	msg_tagged MAIN 0 (fun _ -> "Finishing Universal Solving Process\n");
	timer_stop stats.universal_timing;
	timer_stop stats.overall_timing;
	if options.generate_statistics
	then msg_plain DEFAULT 0 (fun _ -> universal_solve_format_stats stats);
	(solution, strategy);;




let universal_solve options backend game =
	let stats = universal_solve_init_statistics () in
	universal_solve_run options stats backend game;;



let universal_solve_fallback options backend fallback =
	let solver game =
		let (sol, strat) = backend game in
		if ArrayUtils.exists sol (fun _ pl -> pl >= 0)
		then (sol, strat)
		else fallback game
	in
		universal_solve options solver;;


let universal_solve_by_player_solver options solver game =
	let (sol, strat) = universal_solve options (fun g -> solver g 0) game in
	let solved = ref [] in
	let n = Array.length game in
	for i = 0 to n - 1 do
		if sol.(i) = 0 then solved := i::!solved
	done;
	let game' = pg_copy game in
	pg_remove_nodes game' !solved;
	let (sol', strat') = universal_solve options (fun g -> solver g 1) game' in
	for i = 0 to n - 1 do
		if sol'.(i) = 1 then (
			sol.(i) <- 1;
			strat.(i) <- strat'.(i)
		)
	done;
	(sol, strat);;



let universal_solve_trivial verb_level game =
	universal_solve (universal_solve_def_options false verb_level) (fun _ -> ([||], [||])) game;;


let compute_winning_nodes verb_level game strat pl =
	let sol = fst (universal_solve_trivial verb_level (subgame_by_strat_pl game strat pl)) in
	let l = ref [] in
	for i = 0 to (Array.length sol) - 1 do
		if sol.(i) = pl then l := i::!l
	done;
	!l