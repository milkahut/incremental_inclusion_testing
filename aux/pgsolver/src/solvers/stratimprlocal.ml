open Tcsset;;
open Tcsarray;;
open Tcslist;;
open Tcsbasedata;;
open Paritygame;;
open Univsolve;;
open Solvers;;
open Transformations;;
open Basics;;

module LocalSolver = struct
	
	type 'a doubled = 'a * 'a
	
	type node_head =
		(int *                                                (* index *)
		 int *												  (* priority *)
		 int)                                                 (* owning player *)

	type 'a node_body =
		Winning of 				int *                         (* player *)
		           				int                           (* strategy *)
	|	Default of              (bool ref) doubled *          (* expanded w.r.t. players *)
								IntSet.t ref *                (* all edges minus winning nodes *)
	                            IntSet.t ref *                (* known edges for owning player *)
	                            IntSet.t ref *                (* unknown edges for owning player *)
	                            (IntSet.t ref) doubled *      (* back edges *)
	                            'a doubled                    (* data *)
	
	type 'a node =
		node_head *
		'a node_body
	                            
	type 'a state =
		partial_paritygame *								  (* the game *)
		(('a node) IntMap.t) ref *							  (* graph *)
		(IntSet.t ref) doubled *	     	        		  (* expansion sets *)
		(IntSet.t ref *                                       (* added nodes *)
		 IntSet.t ref *                                       (* removed nodes *)
		 IntSet.t ref) doubled *                              (* border nodes *)
		(int -> int -> 'a)                                    (* init node map: pl -> v -> 'a *)
		
	let _by_player player (x,y) =
		if player = 0 then x else y
		
	let format_state ((_,gr,exp,change,_): 'a state) (f: 'a -> string) =
		let (exp0, exp1) = exp in
		let ((add0,rem0,bor0),(add1,rem1,bor1)) = change in
		let win0 = ref [] in
		let win1 = ref [] in
		let nod0 = ref [] in
		let nod1 = ref [] in
		IntMap.iter (fun _ x ->
			match x with
				((idx,_,_), Winning (pl, str)) -> (
					let s = if pl = 0 then win0 else win1 in
					s := (if str < 0 then string_of_int idx else string_of_int idx ^ "->" ^ string_of_int str)::!s
				)
			|	((idx,_,_), Default (exp,_,_,_,_,(data0,data1))) -> (
					if !(fst exp) then nod0 := (string_of_int idx ^ " with " ^ f data0)::!nod0;
					if !(snd exp) then nod1 := (string_of_int idx ^ " with " ^ f data1)::!nod1
				)
		) !gr;
		"Expansion   0 : " ^ IntSetUtils.format !exp0 ^ "\n" ^
		"Expansion   1 : " ^ IntSetUtils.format !exp1 ^ "\n" ^
		"Added       0 : " ^ IntSetUtils.format !add0 ^ "\n" ^ 
		"Added       1 : " ^ IntSetUtils.format !add1 ^ "\n" ^ 
		"Removed     0 : " ^ IntSetUtils.format !rem0 ^ "\n" ^ 
		"Removed     1 : " ^ IntSetUtils.format !rem1 ^ "\n" ^ 
		"Border      0 : " ^ IntSetUtils.format !bor0 ^ "\n" ^ 
		"Border      1 : " ^ IntSetUtils.format !bor1 ^ "\n" ^
		"Winning     0 : " ^ ListUtils.format (fun s -> s) !win0 ^ "\n" ^ 
		"Winning     1 : " ^ ListUtils.format (fun s -> s) !win1 ^ "\n" ^
		"Graph       0 : " ^ ListUtils.format (fun s -> s) !nod0 ^ "\n" ^ 
		"Graph       1 : " ^ ListUtils.format (fun s -> s) !nod1 ^ "\n"
				
	let _touch (((_,_,data,_), gr, _, _, init_node): 'a state) v =
		if not (IntMap.mem v !gr)
		then let (pr, pl) = data v in
		     let node = ((v, pr, pl), Default ((ref false, ref false), ref IntSet.empty,
		                  ref IntSet.empty, ref IntSet.empty, (ref IntSet.empty, ref IntSet.empty),
		                  (init_node 0 v, init_node 1 v))) in
		     gr := IntMap.add v node !gr
		
	let init_state ((start, _, _, _) as pg) init_node =
		let state = (pg, ref IntMap.empty, (ref (IntSet.singleton start), ref (IntSet.singleton start)),
		             ((ref IntSet.empty, ref IntSet.empty, ref IntSet.empty),
		              (ref IntSet.empty, ref IntSet.empty, ref IntSet.empty)),
		             init_node) in
		_touch state start;
		state
	
	let _get_node (_, gr, _, _, _) v =
		IntMap.find v !gr
		
	let get_node_info st v =
		fst (_get_node st v)
		
	let _get_node_content st v =
		snd (_get_node st v)
		
	let is_solved state v =
		try
			match _get_node_content state v with
				Winning _ -> true
			|	_ -> false
		with
			Not_found -> false
	
	let get_game (pg,_,_,_,_) =
		pg
	
	let get_solved state v =
		match _get_node_content state v with
			Winning (x,y) -> (x,y)
		|	_ -> raise Not_found

	let _get_def_node st v =
		match _get_node_content st v with
			Default (a,b,c,d,e,f) -> (a,b,c,d,e,f)
		|	_ -> raise (Failure "not default node")
		
	let _get_def_node_with_info st v =
		match _get_node st v with
			(info, Default (a,b,c,d,e,f)) -> (info, (a,b,c,d,e,f))
		|	_ -> raise (Failure "not default node")

	let get_data (st: 'a state) pl v =
		let (_,_,_,_,_,data) = _get_def_node st v in
		_by_player pl data
		
	let get_expansion_set (_,_,exp,_,_) pl =
		!(_by_player pl exp)
		
	let get_edges st pl v =
		let ((_,_,pl'),(_,all,known,_,_,_)) = _get_def_node_with_info st v in
		if pl = pl' then !known else !all

	let get_back_edges st pl v =
		let (_,_,_,_,back,_) = _get_def_node st v in
		!(_by_player pl back)
	
	let pop_added_nodes (_,_,_,change,_) pl =
		let (add,_,_) = _by_player pl change in
		let s = !add in
		add := IntSet.empty;
		s

	let pop_removed_nodes (_,_,_,change,_) pl =
		let (_,rem,_) = _by_player pl change in
		let s = !rem in
		rem := IntSet.empty;
		s
		
	let pop_border_nodes (_,_,_,change,_) pl =
		let (_,_,bor) = _by_player pl change in
		let s = !bor in
		bor := IntSet.empty;
		s

	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSOLVER")
	let _dbg_msg_plain = message

	let winning' (((_,gr,expset,changesets,_) as st): 'a state) player nodes_with_strat forbidden =
		let update_global_sets_remove_node v pl expanded =
			if expanded then (
				let (added,removed,border) = _by_player pl changesets in
				if IntSet.mem v !added then added := IntSet.remove v !added else removed := IntSet.add v !removed;
				border := IntSet.remove v !border
			)
			else (
				let s = _by_player pl expset in
				s := IntSet.remove v !s
			)
		in
		let processed = ref nodes_with_strat in
		let todo = ref IntSet.empty in
		IntMap.iter (fun v _ -> todo := IntSet.add v !todo) nodes_with_strat;
		if not (IntSet.is_empty !todo) then (
			_dbg_msg_tagged 3 (fun _ -> "Winning " ^ IntSetUtils.format !todo ^ " for player " ^ string_of_int player ^ "\n");
			while not (IntSet.is_empty !todo) do
				let v = IntSet.min_elt !todo in
				todo := IntSet.remove v !todo;
				let (exp,all,known,unknown,back,_) = _get_def_node st v in
				update_global_sets_remove_node v 0 !(fst exp);
				update_global_sets_remove_node v 1 !(snd exp);
				IntSet.iter (fun w ->
					let (_,_,_,_,(backw0,backw1),_) = _get_def_node st w in
					backw0 := IntSet.remove v !backw0;
					backw1 := IntSet.remove v !backw1
				) (IntSet.union !unknown (IntSet.union !all !known));
				_dbg_msg_tagged 3 (fun _ -> "Check predecessors of " ^ string_of_int v ^ "...\n");
				IntSet.iter (fun w ->
					if not (IntMap.mem w !processed) then (
						let ((_,_,pl),(exp,all,known,unknown,_,_)) = _get_def_node_with_info st w in
						let attractor_predicate =
							(pl = player) ||
							(!(_by_player (1-pl) exp) && IntSet.for_all (fun u -> IntMap.mem u !processed) !all) ||
							(!(_by_player pl exp) && IntSet.for_all (fun u -> IntMap.mem u !processed) !known && IntSet.is_empty !unknown)
						in
						if attractor_predicate && not (IntMap.mem w forbidden) then (
							todo := IntSet.add w !todo;
							processed := IntMap.add w (if pl = player then v else -1) !processed;
							_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int w ^ " owned by player " ^ string_of_int pl ^ " lies in the attractor.\n")
						)
						else (
							if !(fst exp) then (
								let (_,_,border) = _by_player 0 changesets in
							    border := IntSet.add w !border
							);
							if !(snd exp) then (
								let (_,_,border) = _by_player 1 changesets in
							    border := IntSet.add w !border
							);
							all := IntSet.remove v !all;
							known := IntSet.remove v !known;
							unknown := IntSet.remove v !unknown
						)
					)
				) (IntSet.union !(fst back) !(snd back));
			done;
			_dbg_msg_tagged 3 (fun _ -> "Attractor finished. Converting to winning nodes.\n");
			IntMap.iter (fun v w ->
				let head = get_node_info st v in
				gr := IntMap.add v (head, Winning (player, w)) !gr;
			) !processed
		)
	
	let winning st player nodes_with_strat =
		winning' st player nodes_with_strat IntMap.empty

	let expand ((pg,_,expset,changesets,_) as st) player nodes =
		let expset = _by_player player expset in
		let (addedset,removedset,borderset) = _by_player player changesets in
		let (_,delta,_,_) = pg in
		_dbg_msg_tagged 3 (fun _ -> "Expand " ^ IntSetUtils.format nodes ^ " for player " ^ string_of_int player ^ "\n");
		let winning_maps = (ref IntMap.empty, ref IntMap.empty) in
		let nodes = ref nodes in
		while not (IntSet.is_empty !nodes) do
			let v = IntSet.min_elt !nodes in
			nodes := IntSet.remove v !nodes;
			_dbg_msg_tagged 3 (fun _ -> "Subexpand " ^ string_of_int v ^ " for player " ^ string_of_int player ^ "\n");
			let ((_,_,pl),(exp,all,known,unknown,back,_)) = _get_def_node_with_info st v in
			let exp = _by_player player exp in
			if !exp then failwith "already expanded";
			exp := true;
			expset := IntSet.remove v !expset;
			addedset := IntSet.add v !addedset;
			IntSet.iter (fun w ->
				let ((_,_,pl'),(_,_,known',unknown',_,_)) = _get_def_node_with_info st w in
				if pl' = player then (
					known' := IntSet.add v !known';
					unknown' := IntSet.remove v !unknown';
					borderset := IntSet.add w !borderset
				)
			) !(_by_player player back);
			let winning_node = (ref (-1), ref (-1)) in
			List.iter (fun w ->
				_touch st w;
				match _get_node_content st w with
					Winning (winner, _) ->
						let win_node = _by_player winner winning_node in
						win_node := w
				|	Default (expw,_,_,_,backw,_) -> (
						let backw = _by_player player backw in
						let expw = _by_player player expw in
						backw := IntSet.add v !backw;
						if !expw then (
							let succ = if player = pl then known else all in
							succ := IntSet.add w !succ
						)
						else if pl = player then (
							unknown := IntSet.add w !unknown;
							expset := IntSet.add w !expset
						)
						else (
							all := IntSet.add w !all;
							expset := IntSet.add w !expset;
							nodes := IntSet.add w !nodes
						)
					)
			) (Enumerators.to_list (delta v));
			let w = !(_by_player pl winning_node) in
			let w' = !(_by_player (1-pl) winning_node) in
			if w != -1 then (
				let win_map = _by_player pl winning_maps in
			    win_map := IntMap.add v w !win_map;
			    _dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won by player " ^ string_of_int pl ^ " by moving to " ^ string_of_int w ^ "\n")
			)
			else if (w' != -1) &&
			        ((pl = player && IntSet.is_empty !known && IntSet.is_empty !unknown) ||
				     (pl != player && IntSet.is_empty !all)) then (
				let win_map = _by_player (1-pl) winning_maps in
			    win_map := IntMap.add v (-1) !win_map;
			    _dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won by player " ^ string_of_int (1-pl) ^ " because all successors are won by the same player.\n")
			)
		done;
		if not (IntMap.is_empty !(fst winning_maps)) then (
			_dbg_msg_tagged 3 (fun _ -> "Performing winning-by-expansion for player 0.\n");
			winning' st 0 !(fst winning_maps) !(snd winning_maps)
		);
		if not (IntMap.is_empty !(snd winning_maps)) then (
			_dbg_msg_tagged 3 (fun _ -> "Performing winning-by-expansion for player 1.\n");
			winning st 1 !(snd winning_maps)
		)
		
	let iterate_nodes (_,gr,_,_,_) pl f =
		IntMap.iter (fun v (_, body) ->
			match body with
				Default (exp, _, _, _, _, _) ->
					if !(_by_player pl exp) then f v
			|	_ -> ()
		) !gr
		
end;;


module StrategyImprovement = struct

	type 'a doubled = 'a * 'a

	type valuation =
		Top
	|	Bottom of IntSet.t * Int2Set.t

	type node_data =
		int ref *          						(* Strategy *)
		valuation ref      						(* Valuation *)

	type state =
		node_data LocalSolver.state *			(* Local Solver State *)
		(IntSet.t ref) doubled *				(* Nodes to be improved *)
		(IntSet.t ref) doubled                  (* Nodes to be evaluated *)
		
	let _by_player player (x,y) =
		if player = 0 then x else y
		
	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSTRATIMPR")
	let _dbg_msg_plain = message
	
	let _format_valuation = function
		Top -> "TOP"
	|	Bottom (s, _) -> IntSetUtils.format s
	
	let _format_node_data (strat, valu) =
		"(" ^ string_of_int !strat ^ ", " ^ _format_valuation !valu ^ ")"

	let format_state (state,imp,eva) =
		let (imp0, imp1) = imp in
		let (eva0, eva1) = eva in
		LocalSolver.format_state state _format_node_data ^		
		"Improveable 0 : " ^ IntSetUtils.format !imp0 ^ "\n" ^
		"Improveable 1 : " ^ IntSetUtils.format !imp1 ^ "\n" ^
		"Evaluatable 0 : " ^ IntSetUtils.format !eva0 ^ "\n" ^ 
		"Evaluatable 1 : " ^ IntSetUtils.format !eva1 ^ "\n"
		
	let _compare_node (_,_,data,_) v w =
		compare (fst (data v), v) (fst (data w), w)
	
	let get_valuation (state, _, _) pl v =
		if v = -1 then Bottom (IntSet.empty, Int2Set.empty)
		else let (_, valu) = LocalSolver.get_data state pl v in !valu
		
	let compute_valuation (state, _, _) pl v = function
		Top -> Top
	|	Bottom (s, t) ->
			if IntSet.mem v s then Top
			else (
				let (_, pr, _) = LocalSolver.get_node_info state v in
				let (without_pr, with_pr) = Int2Set.partition (fun (pr', _) -> pr' != pr) t in
				let t' = if Int2Set.is_empty with_pr
				         then Int2Set.add (pr, 1) without_pr
						 else Int2Set.add (pr, 1 + snd (Int2Set.min_elt with_pr)) without_pr
				in
				Bottom (IntSet.add v s, t')
			)

	let compare_valuation (state, _, _) pl valu valu' =
		match (valu, valu') with
			(Top, _) ->
				if valu' = Top then 0 else 1
		|	(_, Top) ->
				-1
		|	(Bottom (_, t), Bottom (_, t')) -> (
				if Int2Set.equal t t' then 0
				else let ((pr, _) as e) = Int2Set.max_elt (Int2SetUtils.sym_diff t t') in
				     if Int2Set.mem e t
				     then if pr mod 2 = pl then 1 else -1
				     else if pr mod 2 = pl then -1 else 1
			)
	
	let _equal_valuation valu valu' =
		match (valu, valu') with
			(Top, Top) -> true
		|	(Bottom (s, t), Bottom (s', t')) -> IntSet.equal s s' && Int2Set.equal t t'
		|	_ -> false	

	(* Updates the to-be-evaluated set and the improved set s.t.
	   - the t-b-e set contain all new nodes, all border nodes and no removed nodes
	   - the imp set contain no removed nodes *)
	let process_change_set ((state, imp, eva) as st) pl =
		_dbg_msg_tagged 3 (fun _ -> "Processing Change Sets for player " ^ string_of_int pl ^ ".\n");
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		let rem = LocalSolver.pop_removed_nodes state pl in
		let add = LocalSolver.pop_added_nodes state pl in
		let bor = LocalSolver.pop_border_nodes state pl in
		imp := IntSet.diff !imp rem;
		eva := IntSet.union (IntSet.union add bor) (IntSet.diff !eva rem);
		IntSet.iter (fun v ->
			let (strat, valu) = LocalSolver.get_data state pl v in
			if IntSet.mem !strat rem then (
				strat := -1;
				valu := get_valuation st pl (-1)
			)
		) bor
	
	let process_change_sets state =
		process_change_set state 0;
		process_change_set state 1

	(* Sets the improvement edge, removes the node from the set of improvable nodes,
	   adds the node to the to-be-evaluated set and updates the respective strategy *)
	let improve (state, imp, eva) pl edges =
		_dbg_msg_tagged 3 (fun _ -> "Improve edges " ^ Int2SetUtils.format edges ^ " for player " ^ string_of_int pl ^ ".\n");
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		Int2Set.iter (fun (i,j) ->
			imp := IntSet.remove i !imp;
			eva := IntSet.add i !eva;
			let (strat, _) = LocalSolver.get_data state pl i in
			strat := j
		) edges

	(* Evaluates the t-b-e-set, updates the improved set and returns winning dominion
	   for player pl *)
	let evaluate (((state, imp, eva) as st): state) pl =
		let imp = _by_player pl imp in
		let eva = _by_player pl eva in
		let todo = ref !eva in
		eva := IntSet.empty;
		if not (IntSet.is_empty !todo) then (
			_dbg_msg_tagged 3 (fun _ -> "Evalute nodes " ^ IntSetUtils.format !todo ^ " for player " ^ string_of_int pl ^ "\n");
			let changedval = ref IntSet.empty in
			while not (IntSet.is_empty !todo) do
				let v = IntSet.min_elt !todo in
				_dbg_msg_tagged 3 (fun _ -> "Checking node " ^ string_of_int v ^ "... ");
				todo := IntSet.remove v !todo;
				let (_,_,pl') = LocalSolver.get_node_info state v in
				let (strat, valu) = LocalSolver.get_data state pl v in
				let (valu', strat') =
					if pl' = pl then (compute_valuation st pl v (get_valuation st pl !strat), !strat)
					else let edges = LocalSolver.get_edges state pl v in
					     let strat' = ref (-1) in
					     let valu' = ref Top in
					     IntSet.iter (fun w ->
					     	let valu'' = compute_valuation st pl v (get_valuation st pl w) in
					     	if (!strat' = -1) || (compare_valuation st pl valu'' !valu' < 0) then (
					     		strat' := w;
					     		valu' := valu''
					     	)
					     ) edges;
					     (!valu', !strat')
				in
				if !strat = strat' && _equal_valuation valu' !valu then (
					_dbg_msg_plain 3 (fun _ -> "keeping valuation " ^ _format_valuation !valu ^ " following node " ^ string_of_int !strat ^ "\n")
				)
				else (
					_dbg_msg_plain 3 (fun _ -> "updating valuation " ^ _format_valuation !valu ^ " to " ^ _format_valuation valu' ^ " following node " ^ string_of_int strat' ^ "\n");
					valu := valu';
					strat := strat';
					changedval := IntSet.add v !changedval;
					todo := IntSet.union !todo (LocalSolver.get_back_edges state pl v)
				)
			done;
			_dbg_msg_tagged 3 (fun _ -> "Extract winning nodes and update improvable set.\n");
			let check = ref !changedval in
			IntSet.iter (fun v -> 
				IntSet.iter (fun w -> check := IntSet.add w !check) (LocalSolver.get_back_edges state pl v);
			) !changedval;
			let winningmap = ref IntMap.empty in
			IntSet.iter (fun v ->
				let (_,_,pl') = LocalSolver.get_node_info state v in
				let (strat, valu) = LocalSolver.get_data state pl v in
				if !valu = Top then (
					imp := IntSet.remove v !imp;
					winningmap := IntMap.add v (if pl' = pl then !strat else -1) !winningmap;
					_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is won.\n");					
				)
				else if pl' = pl then (
					let edges = LocalSolver.get_edges state pl v in
					let valu = get_valuation st pl !strat in
					let improvable = ref false in
				    IntSet.iter (fun w ->
				    	if not (!improvable || w = !strat) then (
					    	let valu' = get_valuation st pl w in
					    	improvable := compare_valuation st pl valu' valu > 0;
				     	)
				    ) edges;
				    if !improvable then (
						_dbg_msg_tagged 3 (fun _ -> "Node " ^ string_of_int v ^ " is improvable.\n");
				    	imp := IntSet.add v !imp
				    )
				    else imp := IntSet.remove v !imp
				)
			) !check;
			!winningmap
		)
		else IntMap.empty
		
	let get_local_solver_state (state, _, _) =
		state
	
	let get_improvement_set (_, imp, _) pl =
		!(_by_player pl imp)
		
	let init_state pg =
		let init_node _ _ = (ref (-1), ref (Bottom (IntSet.empty, Int2Set.empty))) in
		let state = LocalSolver.init_state pg init_node in
		(state, (ref IntSet.empty, ref IntSet.empty), (ref IntSet.empty, ref IntSet.empty))
		
	let stable_win (state, _, _) pl =
		let mp = ref IntMap.empty in
		LocalSolver.iterate_nodes state pl (fun v ->
			let (_, _, pl') = LocalSolver.get_node_info state v in
			let strat =
				if pl != pl' then !(fst (LocalSolver.get_data state pl v))
				else -1
			in
			mp := IntMap.add v strat !mp
		);
		LocalSolver.winning state (1-pl) !mp
	
end;;

module StrategyImprovementMain = struct

	type switch_players_policy = StrategyImprovement.state -> int -> int (* data -> current player -> next player *)
	
	type improvement_policy = StrategyImprovement.state -> int -> Int2Set.t (* data -> player -> (node, succ) set *)
	
	type expansion_policy = StrategyImprovement.state -> int -> IntSet.t (* data -> player -> new nodes *)
	
	let _dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSTRATIMPRMAIN")
	let _dbg_msg_tagged_nl v = message_autotagged_newline v (fun _ -> "LOCALSTRATIMPRMAIN")
	let _dbg_msg_plain = message

	let custom_solve (pg: partial_paritygame)
	                 (switch_pol: switch_players_policy)
	                 (expand_pol: expansion_policy)
	                 (improvement_pol: improvement_policy) =
	                 
	    let state = StrategyImprovement.init_state pg in
	    let state' = StrategyImprovement.get_local_solver_state state in
	    let current_player = ref (switch_pol state (-1)) in
	    let iteration = ref 0 in
		let (start,_,_,_) = pg in
	    
		while not (LocalSolver.is_solved state' start) do
			incr iteration;
			_dbg_msg_tagged 3 (fun _ -> "Iteration " ^ string_of_int !iteration ^ " with current player " ^ string_of_int !current_player ^ "\n");
			_dbg_msg_tagged_nl 3 (fun _ -> StrategyImprovement.format_state state);
			let imp = StrategyImprovement.get_improvement_set state !current_player in
			let exp = LocalSolver.get_expansion_set state' !current_player in
			if (IntSet.is_empty imp) && (IntSet.is_empty exp) then (
				_dbg_msg_tagged 3 (fun _ -> "Strategy is stable, other player wins the known graph.\n");
				StrategyImprovement.stable_win state !current_player
			)
			else (
				if IntSet.is_empty imp then (
					_dbg_msg_tagged 3 (fun _ -> "Improvement set is empty, expand.\n");
					LocalSolver.expand state' !current_player (expand_pol state !current_player);
					StrategyImprovement.process_change_sets state;
				)
				else (
					_dbg_msg_tagged 3 (fun _ -> "Improving.\n");
					StrategyImprovement.improve state !current_player (improvement_pol state !current_player);
					current_player := switch_pol state !current_player
				);
				_dbg_msg_tagged 3 (fun _ -> "Updating.\n");
				let changed0 = ref true in
				let changed1 = ref true in
				while !changed0 || !changed1 do
					changed0 := false;
					let win0 = StrategyImprovement.evaluate state 0 in
					if not (IntMap.is_empty win0) then (
						changed0 := true;
						LocalSolver.winning state' 0 win0;
						StrategyImprovement.process_change_sets state
					);
					changed1 := false;
					let win1 = StrategyImprovement.evaluate state 1 in
					if not (IntMap.is_empty win1) then (
						changed1 := true;
						LocalSolver.winning state' 1 win1;
						StrategyImprovement.process_change_sets state
					);
				done;
			)
		done;
		_dbg_msg_tagged_nl 3 (fun _ -> StrategyImprovement.format_state state);
	
	let sol i =
		try
	 		let (win, strat) = LocalSolver.get_solved state' i in
			if strat = -1 then (win, None) else (win, Some strat)
	 	with
	 		Not_found -> (-1, None)
	in
	
	sol


	let default_switch_players _ pl =
		if pl < 0 then 0 else 1 - pl
	
	let default_expansion_policy st pl =
		let st' = StrategyImprovement.get_local_solver_state st in
		let s = IntSet.elements (LocalSolver.get_expansion_set st' pl) in
		let a = Array.of_list s in
		IntSet.singleton (a.(Random.int (Array.length a)))
		(*
		IntSet.singleton (ListUtils.max_elt (fun i j ->
			let (_, pri, _) = LocalSolver.get_node_info st' i in
			let (_, prj, _) = LocalSolver.get_node_info st' j in
			let rewi = if pri mod 2 = pl then pri else -pri in
			let rewj = if prj mod 2 = pl then prj else -prj in
			compare rewi rewj
		) s)
		*)
		
	let default_improvement_policy st pl =
		let st' = StrategyImprovement.get_local_solver_state st in
		let s = IntSet.elements (StrategyImprovement.get_improvement_set st pl) in
		let s' = List.map (fun i ->
			let edges = LocalSolver.get_edges st' pl i in
			let j = ref (-1) in
			IntSet.iter (fun k ->
				let valuk = StrategyImprovement.get_valuation st pl k in
				let valuj = StrategyImprovement.get_valuation st pl !j in
				if StrategyImprovement.compare_valuation st pl valuk valuj > 0 then j := k
			) edges;
			(i, !j)
		) s in
		Int2SetUtils.of_list s'
		(*
		let i = IntSet.min_elt (StrategyImprovement.get_improvement_set st pl) in
		let edges = LocalSolver.get_edges st' pl i in
		let j = ref (-1) in
		IntSet.iter (fun k ->
			let valuk = StrategyImprovement.get_valuation st pl k in
			let valuj = StrategyImprovement.get_valuation st pl !j in
			if StrategyImprovement.compare_valuation st pl valuk valuj > 0 then j := k
		) edges;
		Int2Set.singleton (i, !j)
		*)
		
	let default_solve (pg: partial_paritygame) =
		Random.self_init ();
		custom_solve pg default_switch_players default_expansion_policy default_improvement_policy
	
end;;
	
let partially_solve pg =
	partialpg_alternating_revertive_restriction (StrategyImprovementMain.default_solve (partialpg_alternating_transformation pg));;
	
register_partial_solver partially_solve "stratimprlocal" "sl" "use the local strategy improvement method [experimental]";;

let solve' game =
	partially_solve_dominion game 0 partially_solve;;	
	
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve' game;;

register_solver solve "stratimprlocal" "sl" "use the local strategy improvement method [experimental]";;