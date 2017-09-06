open Tcsset;;
open Tcsarray;;
open Tcslist;;
open Tcsgraph;;
open Tcsbasedata;;
open Tcstiming;;
open Paritygame;;
open Univsolve;;
open Solvers;;
open Transformations;;
open Basics;;

(* Unoptimized implementation *)

type node_data = {
	index: int;
	priority: int;
	owner: int;
	mutable strategy: node_data Digraph.node;
	mutable unexpanded: IntSet.t;
	mutable valuation: valuation_type;
}
and valuation_type = (node_data Digraph.node) * (node_data Digraph.nodes)

type global_data = {
	game:                   partial_paritygame;
	graph:                  node_data Digraph.digraph;
	mutable exp_back_edges: (node_data Digraph.nodes) IntMap.t;
	mutable index_to_node:  (node_data Digraph.node) IntMap.t;
	mutable improvable0:    node_data Digraph.nodes;
	mutable improvable1:    node_data Digraph.nodes;
	mutable expandable0:    node_data Digraph.nodes;
	mutable expandable1:    node_data Digraph.nodes;
	mutable inconsistent:   node_data Digraph.nodes;
	mutable relevantset:    node_data Digraph.nodes;
	mutable relevantplayer: int;
}

type improve_policy = global_data -> node_data Digraph.nodes -> (node_data Digraph.node * node_data Digraph.node) list

type expand_policy = global_data -> node_data Digraph.nodes -> int list

let format_node node =
	let idx_fmt node = string_of_int (Digraph.get_content node).index in
	let content = Digraph.get_content node in
	string_of_int content.index ^ " " ^
	string_of_int content.priority ^ " " ^
	string_of_int content.owner ^ " " ^
	idx_fmt content.strategy ^ " " ^
	IntSetUtils.format content.unexpanded ^ " " ^
	idx_fmt (fst content.valuation) ^ " " ^
	PlainsetUtils.format idx_fmt (snd content.valuation) ^ " " ^
	PlainsetUtils.format idx_fmt (Digraph.get_fwd_edges node) ^ " " ^
	PlainsetUtils.format idx_fmt (Digraph.get_bwd_edges node);;
	
let format_global_state gd =
	let s = ref "" in
	IntMap.iter (fun _ node -> s := !s ^ format_node node ^ "\n") gd.index_to_node;
	let idx_fmt node = string_of_int (Digraph.get_content node).index in
	"\n" ^ !s ^ "\n" ^
	"Improvable0  = " ^ PlainsetUtils.format idx_fmt gd.improvable0 ^ "\n" ^
	"Improvable1  = " ^ PlainsetUtils.format idx_fmt gd.improvable1 ^ "\n" ^
	"Expandable0  = " ^ PlainsetUtils.format idx_fmt gd.expandable0 ^ "\n" ^
	"Expandable1  = " ^ PlainsetUtils.format idx_fmt gd.expandable1 ^ "\n" ^
	"Inconsistent = " ^ PlainsetUtils.format idx_fmt gd.inconsistent ^ "\n" ^
	"RelevantSet  = " ^ PlainsetUtils.format idx_fmt gd.relevantset ^ "\n" ^
	"RelevantPlay = " ^ string_of_int gd.relevantplayer ^ "\n\n";;

let dbg_msg_tagged v = message_autotagged v (fun _ -> "LOCALSOLVER2")
let dbg_msg_plain = message

let compare_node_reward player v w =
	let vcont = Digraph.get_content v in
	let wcont = Digraph.get_content w in
	let rewv = if vcont.priority mod 2 = player then vcont.priority else -vcont.priority in
	let reww = if wcont.priority mod 2 = player then wcont.priority else -wcont.priority in
	let c = compare rewv reww in
	if c != 0 then c
	else if vcont.priority mod 2 = player
	then compare vcont.index wcont.index
	else compare wcont.index vcont.index

let compare_node_relevance v w =	
	let vcont = Digraph.get_content v in
	let wcont = Digraph.get_content w in
	let c = compare vcont.priority wcont.priority in
	if c != 0 then c
	else compare vcont.index wcont.index
	
let compare_valuation player v w =
	let contv = Digraph.get_content v in
	let contw = Digraph.get_content w in
	let (cyclev, pathv) = contv.valuation in
	let (cyclew, pathw) = contw.valuation in
	let prcv = (Digraph.get_content cyclev).priority in
	let prcw = (Digraph.get_content cyclew).priority in
	if (prcv mod 2 = player) && (prcw mod 2 = player) && (player = 0) then 0
	else
	let c = compare_node_reward player cyclev cyclew in
	if c != 0 then c else (
		let cycleplayer = (Digraph.get_content cyclev).priority mod 2 in
		let best = ref cyclev in
		Plainset.iter (fun node ->
			if compare_node_relevance node !best > 0 then best := node
		) (Plainset.sym_diff pathv pathw);
		if compare_node_relevance !best cyclev > 0
		then (if Plainset.mem !best pathv then 1 else -1) *
			 (if (Digraph.get_content !best).priority mod 2 = player then 1 else -1)
		else let c = compare (Plainset.cardinal pathv) (Plainset.cardinal pathw) in
			 if cycleplayer = player then -c else c
	)


let evaluate (gd: global_data) =
	dbg_msg_tagged 3 (fun _ -> "EVALUATE\n");
	(* Update Valuations *)
	let todo = ref (Digraph.empty_nodes ()) in
	(* Collect all affected nodes *)
	while not (Plainset.is_empty gd.inconsistent) do
		let v = Plainset.min_elt gd.inconsistent in
		gd.inconsistent <- Plainset.remove v gd.inconsistent;
		if not (Plainset.mem v !todo) then (
			todo := Plainset.add v !todo;
			Plainset.iter (fun w ->
				if (not (Plainset.mem w !todo)) && (Digraph.equal_node (Digraph.get_content w).strategy v) then (
					gd.inconsistent <- Plainset.add w gd.inconsistent
				)
			) (Digraph.get_bwd_edges v);
		);
	done;
	let changed = !todo in
	(* Work through all affected nodes *)
	while not (Plainset.is_empty !todo) do
		let current = ref (Plainset.min_elt !todo) in
		todo := Plainset.remove !current !todo;
		let memory = ref (Plainset.add !current (Digraph.empty_nodes ())) in
		let next = ref ((Digraph.get_content !current).strategy) in
		while (Plainset.mem !next !todo) do
			current := !next;
			todo := Plainset.remove !current !todo;
			memory := Plainset.add !current !memory;
			next := (Digraph.get_content !current).strategy
		done;
		current := !next;
		(* Check whether we encountered an inconsistent cycle *)
		if Plainset.mem !current !memory then (
			let best = ref !current in
			while (Plainset.mem !current !memory) do
				if compare_node_relevance !current !best > 0 then best := !current;
				memory := Plainset.remove !current !memory;
				current := (Digraph.get_content !current).strategy
			done;
			current := !best;
			(Digraph.get_content !current).valuation <-
				(!current, Digraph.empty_nodes ())
		);
		(* Now current contains the first valid node. Propagate backwards! *)
		let valued = ref (Digraph.empty_nodes ()) in
		let rec prop_backwards node =
			if not (Plainset.mem node !valued) then (
				valued := Plainset.add node !valued;
				todo := Plainset.remove node !todo;
				let (cycle, path) = (Digraph.get_content node).valuation in
				Plainset.iter (fun w ->
					if (not (Plainset.mem w !valued)) &&
					   (Digraph.equal_node (Digraph.get_content w).strategy node) &&
					   (Plainset.mem w changed) then (
						(Digraph.get_content w).valuation <- (cycle, Plainset.add w path);
						prop_backwards w
					)
				) (Digraph.get_bwd_edges node)
				
			)
		in
		prop_backwards !current
	done;
	(* Update Improvement Sets *)
	let todo = ref (Digraph.empty_nodes ()) in
	Plainset.iter (fun node ->
		todo := Plainset.add node !todo;
		Plainset.iter (fun node' ->
			todo := Plainset.add node' !todo
		) (Digraph.get_bwd_edges node)
	) changed;
	Plainset.iter (fun node ->
		let content = Digraph.get_content node in
		if Plainset.exists (fun node' -> compare_valuation content.owner node' content.strategy > 0) (Digraph.get_fwd_edges node) then (
			if content.owner = 0
			then gd.improvable0 <- Plainset.add node gd.improvable0
			else gd.improvable1 <- Plainset.add node gd.improvable1;
		)
		else (
			if content.owner = 0
			then gd.improvable0 <- Plainset.remove node gd.improvable0
			else gd.improvable1 <- Plainset.remove node gd.improvable1;
		) 
	) !todo;
	(* Update Relevant Player *)
	let (start, _, _, _) = gd.game in
	let start_node = IntMap.find start gd.index_to_node in
	let start_content = Digraph.get_content start_node in
	gd.relevantplayer <- 1 - (Digraph.get_content (fst start_content.valuation)).priority mod 2;
	(* Update Relevant Set *)
	gd.relevantset <- Digraph.empty_nodes ();
	let todo = ref [start_node] in
	while !todo != [] do
		let current = List.hd !todo in
		todo := List.tl !todo;
		if not (Plainset.mem current gd.relevantset) then (
			gd.relevantset <- Plainset.add current gd.relevantset;
			let content = Digraph.get_content current in
			if content.owner != gd.relevantplayer then todo := content.strategy::!todo
			else todo := Plainset.elements (Digraph.get_fwd_edges current) @ !todo
		)
	done;;
	

	
let expand (gd: global_data) nodes expand_policy =
	dbg_msg_tagged 3 (fun _ -> "EXPAND\n");
	let (_, delta, data, _) = gd.game in
	let todo = ref nodes in
	let todo_strategy = ref [] in
	while !todo != [] do
		let idx = List.hd !todo in
		todo := List.tl !todo;
		if not (IntMap.mem idx gd.index_to_node) then (
			let node = Digraph.add_empty_node gd.graph in
			todo_strategy := node::!todo_strategy;
			let bwd = IntMap.find idx gd.exp_back_edges in
			gd.exp_back_edges <- IntMap.remove idx gd.exp_back_edges;
			gd.inconsistent <- Plainset.add node gd.inconsistent;
			gd.index_to_node <- IntMap.add idx node gd.index_to_node;
			let (pr, pl) = data idx in
			let (known, unknown) = List.partition (fun i -> IntMap.mem i gd.index_to_node) (Enumerators.to_list (delta idx)) in
			let content = {
				index = idx;
				priority = pr;
				owner = pl;
				strategy = node;
				unexpanded = IntSetUtils.of_list unknown;
				valuation = (node, Digraph.empty_nodes ())
			} in
			Digraph.set_content node content;
			Plainset.iter (fun node' ->
				Digraph.add_edge gd.graph node' node;
				let content' = Digraph.get_content node' in
				content'.unexpanded <- IntSet.remove idx content'.unexpanded;
				if IntSet.is_empty content'.unexpanded then (
					if content'.owner = 0
					then gd.expandable0 <- Plainset.remove node' gd.expandable0
					else gd.expandable1 <- Plainset.remove node' gd.expandable1
				)
			) bwd;
			List.iter (fun node' ->
				Digraph.add_edge gd.graph node (IntMap.find node' gd.index_to_node);
			) known;
			List.iter (fun node' ->
				let back =
					try
						IntMap.find node' gd.exp_back_edges
					with
						Not_found -> Digraph.empty_nodes ()
				in
				gd.exp_back_edges <- IntMap.add node' (Plainset.add node back) gd.exp_back_edges
			) unknown;
			if unknown != [] then (
				if pl = 0
				then gd.expandable0 <- Plainset.add node gd.expandable0
				else gd.expandable1 <- Plainset.add node gd.expandable1
			);
			if known = [] then todo := expand_policy gd (Plainset.add node (Digraph.empty_nodes ())) @ !todo
		)
	done;
	while !todo_strategy != [] do
		let node = List.hd !todo_strategy in
		todo_strategy := List.tl !todo_strategy;
		let content = Digraph.get_content node in
		let fwd = ref (Digraph.get_fwd_edges node) in
		let best = ref (Plainset.min_elt !fwd) in
		fwd := Plainset.remove !best !fwd;
		while not (Plainset.is_empty !fwd) do
			let cur = Plainset.min_elt !fwd in
			fwd := Plainset.remove cur !fwd;
			if compare_valuation content.owner !best cur < 0
			then best := cur
		done;
		content.strategy <- !best
	done;;


let improve (gd: global_data) nodes =
	dbg_msg_tagged 3 (fun _ -> "IMPROVE\n");
	List.iter (fun (v, w) ->
		(Digraph.get_content v).strategy <- w;
		gd.inconsistent <- Plainset.add v gd.inconsistent
	) nodes
	
	
let init_global_data pg =
	let (start, _, _, _) = pg in
{
	game = pg;
	graph = Digraph.create ();
	exp_back_edges = IntMap.add start (Digraph.empty_nodes ()) IntMap.empty;
	index_to_node = IntMap.empty;
	improvable0 = Digraph.empty_nodes ();
	improvable1 = Digraph.empty_nodes ();
	expandable0 = Digraph.empty_nodes ();
	expandable1 = Digraph.empty_nodes ();
	inconsistent = Digraph.empty_nodes ();
	relevantset = Digraph.empty_nodes ();
	relevantplayer = 0;
}

let assemble_solution gd i =
	try
		let node = IntMap.find i gd.index_to_node in
		let content = Digraph.get_content node in
		(1 - gd.relevantplayer, (if 1 - gd.relevantplayer = content.owner then Some (Digraph.get_content content.strategy).index else None))
	with
		Not_found -> (-1, None)

let solve_locally (pg: partial_paritygame)
                  (impr: improve_policy)
				  (exp: expand_policy) =

	let gd = init_global_data pg in
	
	let (start, _, _, _) = pg in
	expand gd [start] exp;
	let finished = ref false in
	while not !finished do
		dbg_msg_tagged 3 (fun _ -> format_global_state gd);
		if not !finished then evaluate gd;
		dbg_msg_tagged 3 (fun _ -> format_global_state gd);
		let rel_impr1 = (*Plainset.inter gd.relevantset*) gd.improvable1 in
		if not (Plainset.is_empty rel_impr1)
		then improve gd (impr gd rel_impr1)
		else let rel_impr0 = (*Plainset.inter gd.relevantset*) gd.improvable0 in
		     if not (Plainset.is_empty rel_impr0)
		     then improve gd (impr gd rel_impr0)
		     else let expset = if gd.relevantplayer = 0 then gd.expandable0 else gd.expandable1 in
		          let rel_exp = Plainset.inter gd.relevantset expset in
		          if not (Plainset.is_empty rel_exp)
		          then expand gd (exp gd rel_exp) exp
		          else finished := true;
	done;
	assemble_solution gd;;


let default_improvement_policy gd nodes =
	List.map (fun node ->
		let cont = Digraph.get_content node in
		let current = ref cont.strategy in
		Plainset.iter (fun node' ->
			if compare_valuation cont.owner !current node' < 0
			then current := node'
		) (Digraph.get_fwd_edges node);
		(node, !current)
	) (Plainset.elements nodes);;
	
	
let default_expansion_policy gd nodes =
	[IntSet.min_elt (Digraph.get_content (Plainset.min_elt nodes)).unexpanded];;
	
	
let partially_solve pg =
	solve_locally pg default_improvement_policy default_expansion_policy;;
	
	
register_partial_solver partially_solve "stratimprloc2" "sl2" "use the local strategy improvement method 2 [experimental]";;

let solve_globally game =
	partially_solve_dominion game 0 partially_solve;;	
	
let solve game = universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) solve_globally game;;

register_solver solve "stratimprloc2" "sl2" "use the local strategy improvement method 2 [experimental]";;
