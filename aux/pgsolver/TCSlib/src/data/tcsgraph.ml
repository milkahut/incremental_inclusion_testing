open Tcslist;;
open Tcsarray;;
open Tcsset;;
open Tcsbasedata;;
open Tcshash;;



module GraphUtils = struct

	type 'a undirected_root_graph = 'a * ('a -> 'a Iterators.iterator)
	
	type 'a directed_root_graph = 'a * ('a -> 'a Iterators.iterator) * ('a -> 'a Iterators.iterator)
	
	type 'a occurrence = 'a -> bool
	
	let default_occurrence_func cmp =
		let base = ref (Plainset.empty cmp) in
		fun x -> if Plainset.mem x !base then true
		         else (base := Plainset.add x !base; false)

	let rec is_reachable2 occ (init, iter) pred =
		if occ init then false else if pred init then true
		else Iterators.exists (iter init) (fun n -> is_reachable2 occ (n, iter) pred)
	
	let is_reachable (gr: 'a undirected_root_graph) =
		is_reachable2 (default_occurrence_func compare) gr
	
	let iterate_with_minimal_distance2 occ (init, iter) cb =
		let rec iterate i stack newstack =
			match stack with
				l::ls -> (
					match l with
						n::ns -> 
							if occ n then iterate i (ns::ls) newstack
							else (
								cb n i;
								iterate i (ns::ls) ((Iterators.to_list (iter n))::newstack)
							)
					|	[] -> iterate i ls newstack
				)
			|	[] -> (
				match newstack with
					[] -> ()
				|	_ -> iterate (i + 1) newstack []
			)
		in
			iterate 0 [[init]] []
			
	let iterate_with_minimal_distance (gr: 'a undirected_root_graph) =
		iterate_with_minimal_distance2 (default_occurrence_func compare) gr
		
	let iterate_with_maximal_distance_single_loop2 occ (init, prev, next) cb =
		let get_dist' u n =
			if n = init then 0
			             else 1 + ListUtils.max_elt compare (List.map u (Iterators.to_list (next n)))
		in
		let get_dist = HashUtils.hash_function_plain get_dist' in
		let rec reach_next i n =
			if not (occ n) then (
				cb n (get_dist n);
				Iterators.iter (prev n) (reach_next (i + 1))
			)
		in
			reach_next 0 init
	
	let iterate_with_maximal_distance_single_loop (gr: 'a directed_root_graph) =
		iterate_with_maximal_distance_single_loop2 (default_occurrence_func compare) gr
			
	let build_reachability_set2 cmp (init, iter) =
		let s = ref (Plainset.empty cmp) in
		let rec iterate n =
			if not (Plainset.mem n !s) then (
				s := Plainset.add n !s;
				Iterators.iter (iter n) iterate
			)
		in
			iterate init;
			!s
		
	let build_reachability_set (gr: 'a undirected_root_graph) =
		build_reachability_set2 compare gr

end

module Digraph = struct

	type 'a node' = {
		mutable content: 'a option;
		mutable fwd_edges: 'a nodes;
		mutable bwd_edges: 'a nodes;
	}
	and 'a node = ('a node') CompRef.compref
	and 'a nodes = ('a node) Plainset.plainset
	
	type 'a digraph = {
		mutable nodes: 'a nodes;
		mutable node_count: int;
		mutable edge_count: int;
	}
	
	let compare_node = CompRef.compare
	
	let equal_node = CompRef.equal
	
	let empty_nodes _ =
		Plainset.empty compare_node
		
	let create _ = {
		nodes = empty_nodes ();
		node_count = 0;
		edge_count = 0;
	}
	
	let node_count gr =
		gr.node_count
		
	let edge_count gr =
		gr.edge_count
		
	let add_empty_node gr =
		let node = CompRef.newref {
			content = None;
			fwd_edges = Plainset.empty compare_node;
			bwd_edges = Plainset.empty compare_node;
		} in
		gr.nodes <- Plainset.add node gr.nodes;
		gr.node_count <- gr.node_count + 1;
		node
		
	let del_node gr node =
		if Plainset.mem node gr.nodes then (
			let nodederef = CompRef.getref node in
			Plainset.iter (fun node' ->
				let node' = CompRef.getref node' in
				node'.bwd_edges <- Plainset.remove node node'.bwd_edges
			) nodederef.fwd_edges;
			Plainset.iter (fun node' ->
				let node' = CompRef.getref node' in
				node'.fwd_edges <- Plainset.remove node node'.fwd_edges
			) nodederef.bwd_edges;
			gr.nodes <- Plainset.remove node gr.nodes;
			gr.node_count <- gr.node_count - 1;
			gr.edge_count <- gr.edge_count - Plainset.cardinal nodederef.fwd_edges - Plainset.cardinal nodederef.bwd_edges
		)

	let add_edge gr nodex nodey =
		let nodexderef = CompRef.getref nodex in
		if not (Plainset.mem nodey nodexderef.fwd_edges) then (
			nodexderef.fwd_edges <- Plainset.add nodey nodexderef.fwd_edges;
			let nodeyderef = CompRef.getref nodey in
			nodeyderef.bwd_edges <- Plainset.add nodex nodeyderef.bwd_edges;
			gr.edge_count <- gr.edge_count + 1
		)
		
	let del_edge gr nodex nodey =
		let nodexderef = CompRef.getref nodex in
		if Plainset.mem nodey nodexderef.fwd_edges then (
			let nodeyderef = CompRef.getref nodey in
			nodeyderef.bwd_edges <- Plainset.remove nodex nodeyderef.bwd_edges;
			gr.edge_count <- gr.edge_count - 1
		)
	
	let get_content node =
		OptionUtils.get_some (CompRef.getref node).content

	let set_content node content =
		(CompRef.getref node).content <- Some content
	
	let get_fwd_edges node =
		(CompRef.getref node).fwd_edges
		
	let get_bwd_edges node =
		(CompRef.getref node).bwd_edges
		
	let get_nodes gr =
		gr.nodes

	let add_node gr content =
		let node = add_empty_node gr in
		set_content node content;
		node

end;;

(*
module type DigraphOptions = sig
	val check_consistency: bool
end

module type Digraph = sig
	exception DigraphException of string

	type node_type
	
	type digraph
	type digraph_node
	type digraph_edge

	val nil_node: digraph_node
	val nil_edge: digraph_edge
	val nil_graph: digraph

	val is_nil_node: digraph_node -> bool
	val is_nil_edge: digraph_edge -> bool
	val is_nil_graph: digraph -> bool
	
	val create: unit -> digraph
	
	val get_node_count: digraph -> int
	val get_edge_count: digraph -> int
	
	val get_fwd_edge_count: digraph_node -> int
	val get_bwd_edge_count: digraph_node -> int
	
	val get_first_node: digraph -> digraph_node
	val get_last_node: digraph -> digraph_node
	val get_next_node: digraph_node -> digraph_node
	val get_prev_node: digraph_node -> digraph_node
	
	val get_first_fwd_edge: digraph_node -> digraph_edge
	val get_last_fwd_edge: digraph_node -> digraph_edge
	val get_next_fwd_edge: digraph_edge -> digraph_edge
	val get_prev_fwd_edge: digraph_edge -> digraph_edge
	
	val get_first_bwd_edge: digraph_node -> digraph_edge
	val get_last_bwd_edge: digraph_node -> digraph_edge
	val get_next_bwd_edge: digraph_edge -> digraph_edge
	val get_prev_bwd_edge: digraph_edge -> digraph_edge

	val get_node_data: digraph_node -> node_type
	val set_node_data: digraph_node -> node_type -> unit
	val get_node_graph: digraph_node -> digraph

	val get_edge_source: digraph_edge -> digraph_node
	val get_edge_target: digraph_edge -> digraph_node
	val get_edge_graph: digraph_edge -> digraph
	
	val create_node: node_type -> digraph_node
	
	val add_node2_after: digraph_node -> digraph_node -> unit
	val add_node_after: node_type -> digraph_node -> digraph_node
	
	val add_node2_before: digraph_node -> digraph_node -> unit
	val add_node_before: node_type -> digraph_node -> digraph_node

	val add_node2_as_last: digraph -> digraph_node -> unit
	val add_node_as_last: digraph -> node_type -> digraph_node

	val add_node2_as_first: digraph -> digraph_node -> unit
	val add_node_as_first: digraph -> node_type -> digraph_node
	
	val add_node2: digraph -> digraph_node -> unit
	val add_node: digraph -> node_type -> digraph_node

	val move_node_after: digraph_node -> digraph_node -> unit
	val move_node_before: digraph_node -> digraph_node -> unit
	
	val del_node: digraph_node -> unit
	
	val create_edge: digraph_node -> digraph_node -> digraph_edge
	
	val add_edge2: digraph -> digraph_edge -> unit
	val add_edge: digraph_node -> digraph_node -> digraph_edge

	val del_edge: digraph_edge -> unit
	
	val iterate_nodes_fwd: digraph -> digraph_node iterator_type
	val iterate_nodes_bwd: digraph -> digraph_node iterator_type
	
	val read_node_marker: digraph_node -> int
	val pop_node_marker: digraph_node -> unit
	val push_node_marker: digraph_node -> int -> unit

	val push_node_markers: digraph_node iterator_type -> (digraph_node -> int) -> unit
	val pop_node_markers: digraph_node iterator_type -> unit
	
	val push_indexer: digraph_node iterator_type -> digraph_node indexer_type
	val pop_indexer: digraph_node iterator_type -> unit
	
	val sort_nodes: digraph -> (digraph_node -> digraph_node -> int) -> unit
	
	val iterate_node_fwd_edges_fwd: digraph_node -> digraph_edge iterator_type
	val iterate_node_bwd_edges_fwd: digraph_node -> digraph_edge iterator_type
	val iterate_node_fwd_edges_bwd: digraph_node -> digraph_edge iterator_type
	val iterate_node_bwd_edges_bwd: digraph_node -> digraph_edge iterator_type

	val strongly_connected_components: digraph ->
	                                   digraph_node indexer_type ->
									   (digraph_node list array) * (* sccs *)
									   (int array) *               (* sccindex *)
									   (int list array) *          (* topology *)
									   (int list)                  (* roots *)
end

module MakeDigraph (Init: TypeBox) (Options: DigraphOptions) = struct

	exception DigraphException of string

	type node_type = Init.t

	type digraph_edge' = {
		mutable edgegraph: digraph;
		mutable source: digraph_node;
		mutable target: digraph_node;
		mutable prev_source_edge: digraph_edge;
		mutable next_source_edge: digraph_edge;
		mutable prev_target_edge: digraph_edge;
		mutable next_target_edge: digraph_edge;
	}
	and digraph_edge = digraph_edge' ref
	and digraph_node' = {
		mutable nodegraph: digraph;
		mutable data: node_type;
		mutable first_bwd_edge: digraph_edge;
		mutable last_bwd_edge: digraph_edge;
		mutable bwd_edge_count: int;
		mutable first_fwd_edge: digraph_edge;
		mutable last_fwd_edge: digraph_edge;
		mutable fwd_edge_count: int;
		mutable prev_node: digraph_node;
		mutable next_node: digraph_node;
		mutable marker_stack: int list;
	}
	and digraph_node = digraph_node' ref
	and digraph' = {
		mutable first_node: digraph_node;
		mutable last_node: digraph_node;
		mutable node_count: int;
		mutable edge_count: int;
	}
	and digraph = digraph' ref

	let rec nil_node = ref {
		nodegraph = nil_graph;
		data = Init.default;
		first_bwd_edge = nil_edge;
		last_bwd_edge = nil_edge;
		bwd_edge_count = 0;
		first_fwd_edge = nil_edge;
		last_fwd_edge = nil_edge;
		fwd_edge_count = 0;
		prev_node = nil_node;
		next_node = nil_node;
		marker_stack = [];
	}
	and nil_edge = ref {
		edgegraph = nil_graph;
		source = nil_node;
		target = nil_node;
		prev_source_edge = nil_edge;
		next_source_edge = nil_edge;
		prev_target_edge = nil_edge;
		next_target_edge = nil_edge;
	}
	and nil_graph = ref {
		first_node = nil_node;
		last_node = nil_node;
		node_count = 0;
		edge_count = 0;
	}

	let is_nil_node node = node == nil_node

	let is_nil_edge edge = edge == nil_edge
	
	let is_nil_graph graph = graph == nil_graph
	
	let create _ = ref {
		first_node = nil_node;
		last_node = nil_node;
		node_count = 0;
		edge_count = 0;
	}
		
	let get_node_count graph = (!graph).node_count
	
	let get_edge_count graph = (!graph).edge_count
	
	let get_fwd_edge_count node = !node.fwd_edge_count
	
	let get_bwd_edge_count node = !node.bwd_edge_count
	
	let get_first_node graph = !graph.first_node
	
	let get_last_node graph = !graph.last_node
	
	let get_next_node node = !node.next_node
	
	let get_prev_node node = !node.prev_node
	
	let get_first_fwd_edge node = !node.first_fwd_edge
	
	let get_last_fwd_edge node = !node.last_fwd_edge
	
	let get_next_fwd_edge edge = !edge.next_target_edge
	
	let get_prev_fwd_edge edge = !edge.prev_target_edge
	
	let get_first_bwd_edge node = !node.first_bwd_edge
	
	let get_last_bwd_edge node = !node.last_bwd_edge
	
	let get_next_bwd_edge edge = !edge.next_source_edge
	
	let get_prev_bwd_edge edge = !edge.prev_source_edge

	let get_node_data node = !node.data
	
	let set_node_data node data = !node.data <- data
	
	let get_node_graph node = !node.nodegraph

	let get_edge_source edge = !edge.source
	
	let get_edge_target edge = !edge.target
	
	let get_edge_graph edge = !edge.edgegraph
	
	let create_node data = ref {
		nodegraph = nil_graph;
		data = data;
		first_bwd_edge = nil_edge;
		last_bwd_edge = nil_edge;
		bwd_edge_count = 0;
		first_fwd_edge = nil_edge;
		last_fwd_edge = nil_edge;
		fwd_edge_count = 0;
		prev_node = nil_node;
		next_node = nil_node;
		marker_stack = [];
	}
	
	let assert_raise func text =
		raise (DigraphException ("Digraph." ^ func ^ ": " ^ text ^ "!"))
	
	let assert_unowned func text node =
		if not (is_nil_graph (!node).nodegraph) then assert_raise func text
	
	let assert_owned func text node =
		if is_nil_graph (!node).nodegraph then assert_raise func text
	
	let assert_same_nodegraph func text node1 node2 =
		if not ((!node1).nodegraph == (!node2).nodegraph) then assert_raise func text
	
	let assert_same_graph func text graph1 graph2 =
		if not (graph1 == graph2) then assert_raise func text

	let assert_proper_graph func text graph =
		if is_nil_graph graph then assert_raise func text
	
	let assert_owned_edge func text edge =
		if is_nil_graph !edge.edgegraph then assert_raise func text

	let assert_unowned_edge func text edge =
		if not (is_nil_graph !edge.edgegraph) then assert_raise func text

	let internal_remove nodegraph node =
		if is_nil_node !node.prev_node
		then !nodegraph.first_node <- !node.next_node
		else !(!node.prev_node).next_node <- !node.next_node;
		if is_nil_node !node.next_node
		then !nodegraph.last_node <- !node.prev_node
		else !(!node.next_node).prev_node <- !node.prev_node
	
	let internal_insert_after nodegraph node1 node2 =
		if (!node2).next_node == nil_node then (
			(!nodegraph).last_node <- node1
		)
		else (
			let n = (!node2).next_node in
			(!node1).next_node <- n;
			(!n).prev_node <- node1
		);
		!node1.prev_node <- node2;
		!node2.next_node <- node1
	
	let internal_insert_before nodegraph node1 node2 =
		if !node2.prev_node == nil_node then (
			!nodegraph.first_node <- node1
		)
		else (
			let n = !node2.prev_node in
			!node1.prev_node <- n;
			!n.next_node <- node1
		);
		!node1.next_node <- node2;
		!node2.prev_node <- node1

	let add_node2_after node1 node2 =
		if Options.check_consistency then (
			assert_unowned "add_node2_after" "First node is owned" node1;
			assert_owned "add_node2_after" "Second node is not owned" node2
		);
		let nodegraph = !node2.nodegraph in
		!node1.nodegraph <- nodegraph;
		!nodegraph.node_count <- !nodegraph.node_count + 1;
		internal_insert_after nodegraph node1 node2
	
	let add_node_after data node' =
		let node = create_node data in
		add_node2_after node node';
		node

	let add_node2_before node1 node2 =
		if Options.check_consistency then (
			assert_unowned "add_node2_before" "First node is owned" node1;
			assert_owned "add_node2_before" "Second node is not owned" node2
		);
		let nodegraph = !node2.nodegraph in
		!node1.nodegraph <- nodegraph;
		!nodegraph.node_count <- !nodegraph.node_count + 1;
		internal_insert_before nodegraph node1 node2
	
	let add_node_before data node' =
		let node = create_node data in
		add_node2_before node node';
		node

	let add_node2_as_last graph node =
		if Options.check_consistency then (
			assert_unowned "add_node2_as_last" "node is owned" node;
			assert_proper_graph "add_node2_as_last" "Graph is nil" graph;
		);
		!node.nodegraph <- graph;
		let node_count = !graph.node_count in
		if node_count = 0 then (
			!graph.node_count <- 1;
			!graph.first_node <- node;
			!graph.last_node <- node
		)
		else (
			!graph.node_count <- !graph.node_count + 1;
			!node.prev_node <- !graph.last_node;
			!(!graph.last_node).next_node <- node;
			!graph.last_node <- node
		)
		
	let add_node_as_last graph data =
		let node = create_node data in
		add_node2_as_last graph node;
		node
	
	let add_node2_as_first graph node =
		if Options.check_consistency then (
			assert_unowned "add_node2_as_first" "node is owned" node;
			assert_proper_graph "add_node2_as_first" "Graph is nil" graph;
		);
		!node.nodegraph <- graph;
		let node_count = !graph.node_count in
		if node_count = 0 then (
			!graph.node_count <- 1;
			!graph.first_node <- node;
			!graph.last_node <- node
		)
		else (
			!graph.node_count <- !graph.node_count + 1;
			!node.next_node <- !graph.first_node;
			!(!graph.first_node).prev_node <- node;
			!graph.first_node <- node
		)
		
	let add_node_as_first graph data =
		let node = create_node data in
		add_node2_as_first graph node;
		node
	
	let add_node2 = add_node2_as_last
	
	let add_node = add_node_as_last

	let move_node_after node1 node2 =
		if Options.check_consistency then (
			assert_owned "move_node_after" "First node is not owned" node1;
			assert_owned "move_node_after" "Second node is not owned" node2;
			assert_same_nodegraph "move_node_after" "Both nodes have to be owned by the same list" node1 node2
		);
		let nodegraph = !node1.nodegraph in
		internal_remove nodegraph node1;
		internal_insert_after nodegraph node1 node2

	let move_node_before node1 node2 =
		if Options.check_consistency then (
			assert_owned "move_node_before " "First node is not owned" node1;
			assert_owned "move_node_before " "Second node is not owned" node2;
			assert_same_nodegraph "move_node_before " "Both nodes have to be owned by the same list" node1 node2
		);
		let nodegraph = !node1.nodegraph in
		internal_remove nodegraph node1;
		internal_insert_before nodegraph node1 node2

	let del_node node =
		if Options.check_consistency then (
			assert_owned "del_node " "node is not owned" node
		);
		let nodegraph = !node.nodegraph in
		internal_remove nodegraph node;
		!node.nodegraph <- nil_graph;
		!node.next_node <- nil_node;
		!node.prev_node <- nil_node;
		!nodegraph.node_count <- !nodegraph.node_count - 1
		
		
	let create_edge source target = ref {
		edgegraph = nil_graph;
		source = source;
		target = target;
		prev_source_edge = nil_edge;
		next_source_edge = nil_edge;
		prev_target_edge = nil_edge;
		next_target_edge = nil_edge;
	}
	
	let add_edge2 graph edge =
		let source = !edge.source in
		let target = !edge.target in
		if Options.check_consistency then (
			assert_unowned_edge "add_edge2" "edge is owned" edge;
			assert_proper_graph "add_edge2" "Graph is nil" graph;
			assert_owned "add_edge2" "source is nil" source;
			assert_owned "add_edge2" "target is nil" target;
			assert_same_graph "add_edge2" "source graph does not match edge graph" !source.nodegraph graph;
			assert_same_graph "add_edge2" "target graph does not match edge graph" !target.nodegraph graph;
		);
		!graph.edge_count <- !graph.edge_count + 1;
		!source.fwd_edge_count <- !source.fwd_edge_count + 1;
		!target.bwd_edge_count <- !target.bwd_edge_count + 1;
		!edge.edgegraph <- graph;
		!edge.prev_source_edge <- !source.last_fwd_edge;
		if is_nil_edge !edge.prev_source_edge
		then !source.first_fwd_edge <- edge
		else !(!source.last_fwd_edge).next_source_edge <- edge;
		!source.last_fwd_edge <- edge;
		!edge.prev_target_edge <- !target.last_bwd_edge;
		if is_nil_edge !edge.prev_target_edge
		then !target.first_bwd_edge <- edge
		else !(!target.last_bwd_edge).next_target_edge <- edge;
		!target.last_bwd_edge <- edge
	
	let add_edge source target =
		let edge = create_edge source target in
		add_edge2 !source.nodegraph edge;
		edge

	let del_edge edge =
		if Options.check_consistency then assert_owned_edge "del_edge " "edge is not owned" edge;
		let edgegraph = !edge.edgegraph in
		(* Remove from graph *)
		!edge.edgegraph <- nil_graph;
		!edgegraph.edge_count <- !edgegraph.edge_count - 1;
		(* Remove from source *)
		!(!edge.source).fwd_edge_count <- !(!edge.source).fwd_edge_count - 1;
		if (!edge.prev_source_edge == nil_edge)
		then !(!edge.source).first_fwd_edge <- !edge.next_source_edge
		else !(!edge.prev_source_edge).next_source_edge <- !edge.next_source_edge;
		if (!edge.next_source_edge == nil_edge)
		then !(!edge.source).last_fwd_edge <- !edge.prev_source_edge
		else !(!edge.next_source_edge).prev_source_edge <- !edge.prev_source_edge;
		!edge.prev_source_edge <- nil_edge;
		!edge.next_source_edge <- nil_edge;
		(* Remove from target *)
		!(!edge.target).bwd_edge_count <- !(!edge.target).bwd_edge_count - 1;
		if (!edge.prev_target_edge == nil_edge)
		then !(!edge.target).first_bwd_edge <- !edge.next_target_edge
		else !(!edge.prev_target_edge).next_target_edge <- !edge.next_target_edge;
		if (!edge.next_target_edge == nil_edge)
		then !(!edge.target).last_bwd_edge <- !edge.prev_target_edge
		else !(!edge.next_target_edge).prev_target_edge <- !edge.prev_target_edge;
		!edge.prev_target_edge <- nil_edge;
		!edge.next_target_edge <- nil_edge

	let iterate_nodes_fwd graph = get_iterator !graph.first_node (fun a -> !a.next_node) nil_node

	let iterate_nodes_bwd graph = get_iterator !graph.last_node (fun a -> !a.prev_node) nil_node

	let read_node_marker node = List.hd !node.marker_stack
	
	let pop_node_marker node =
		!node.marker_stack <- List.tl !node.marker_stack
		
	let push_node_marker node marker =
		!node.marker_stack <- marker::!node.marker_stack

	let push_node_markers iterator marker =
		iterate_iterator iterator (fun node -> push_node_marker node (marker node))

	let pop_node_markers iterator =
		iterate_iterator iterator pop_node_marker

	let push_indexer iterator =
		let i = ref 0 in
		iterate_iterator iterator (fun node ->
			push_node_marker node !i;
			incr i
		);
		(!i, read_node_marker)

	let pop_indexer iterator =
		pop_node_markers iterator
				
	let sort_nodes graph compare =
		(* Sorts [node_from, node_from+1, ..., node_from + node_count - 1] and
		   returns (node_first_node, node_last_node) *)
		let rec merge_sort_from node_from node_count =
			if (node_count < 2) then (node_from, node_from) else (
				(* Recursion *)
				let half = node_count / 2 in
				let (first_node, mid_left) = merge_sort_from node_from half in
				let node_next = get_next_node mid_left in
				let (mid_right, last_node) = merge_sort_from node_next (node_count - half) in
				
				(* Merge *)
				let glider_left = ref first_node in
				let glider_right = ref mid_right in
				let finished = ref false in
				let start = ref true in
				let final_first_node = ref first_node in
				let final_last_node = ref last_node in
				
				while (not !finished) do
					let current_left = !glider_left in
					let current_right = !glider_right in
					let sound_ordering = compare current_left current_right <= 0 in
					finished := (current_left == mid_left) || (current_right == last_node);
					if !finished then (
						if not sound_ordering then final_last_node := mid_left
					)
					else if sound_ordering
					then glider_left := get_next_node !glider_left
					else glider_right := get_next_node !glider_right;
					if !start then (
						start := false;
						if not sound_ordering then final_first_node := mid_right
					);
					if not sound_ordering
					then move_node_before current_right current_left
				done;
				(!final_first_node, !final_last_node)
			)
		in
			let (first_node, last_node) = merge_sort_from (get_first_node graph) (get_node_count graph) in
			(!graph).first_node <- first_node;
			(!graph).last_node <- last_node

	let iterate_node_fwd_edges_fwd node = get_iterator !node.first_fwd_edge (fun a -> !a.next_source_edge) nil_edge

	let iterate_node_bwd_edges_fwd node = get_iterator !node.first_bwd_edge (fun a -> !a.next_target_edge) nil_edge

	let iterate_node_fwd_edges_bwd node = get_iterator !node.last_fwd_edge (fun a -> !a.prev_source_edge) nil_edge

	let iterate_node_bwd_edges_bwd node = get_iterator !node.last_bwd_edge (fun a -> !a.prev_target_edge) nil_edge

	let strongly_connected_components graph ((max_index, index_map) as indexer) =
		let l = max_index in
		let iterator = iterate_nodes_fwd graph in
		let fwd_edge_iterator = iterate_node_fwd_edges_fwd in
		let bwd_edge_iterator = iterate_node_bwd_edges_fwd in

		let a = build_indexer iterator indexer in
	
		let dfsnum = Array.make l (-1) in
		let index = Array.make l (-1) in
		let visited = Array.make l false in

		let todo = ref [] in

		let n = ref 0 in
		
		let dfs v =
			let st = Stack.create () in
			Stack.push v st;
			while not (Stack.is_empty st) do
				let u = Stack.pop st in
				let pushed = ref false in
				if not visited.(u) then (
					visited.(u) <- true;
					iterate_iterator (fwd_edge_iterator a.(u)) (fun e ->
						let s = !e.target in
						let w = index_map s in
						if not visited.(w) then (
							if not !pushed then (
								Stack.push u st;
								pushed := true
							);
							Stack.push w st
						)
					)
				);
				if (not !pushed) && (dfsnum.(u) < 0) then (
					dfsnum.(u) <- !n;
					index.(!n) <- u;
					incr n
				)
			done
		in

        iterate_iterator iterator (fun node ->
			let i = index_map node in
			if not visited.(i) then dfs i
		);

		decr n;

		iterate_iterator iterator (fun node -> visited.(index_map node) <- false);

		let sccs = ref [] in
		let topology = Dynarray.create IntSet.empty in
		let scc_index = Array.make l (-1) in
		let next_index = ref 0 in
		let roots = ref IntSet.empty in
		let is_root = ref true in

		while !n >= 0 do
			Dynarray.insert topology !next_index IntSet.empty;
			is_root := true;
			todo := [index.(!n)];
			let scc = ref [] in

			while !todo <> [] do
			  let v = List.hd !todo in
			  todo := List.tl !todo;

			  if not visited.(v) && dfsnum.(v) >= 0 then (
			        visited.(v) <- true;
					scc := a.(v) :: !scc;
					let nexts = ref [] in
					iterate_iterator (bwd_edge_iterator a.(v)) (fun e ->
						let s = !e.source in
						let w = index_map s in
						nexts := w::!nexts;
						let c = scc_index.(w) in
						if c > -1 then (
							Dynarray.set topology c (IntSet.add !next_index (Dynarray.get topology c));
							is_root := false
						)
					);
					let nexts = List.sort (fun x -> fun y -> (-1) * (compare dfsnum.(x) dfsnum.(y))) !nexts in 
					todo := nexts @ !todo
				)
			done;
			sccs := !scc::!sccs;
			if !is_root then roots := IntSet.add !next_index !roots;
			List.iter (fun v -> scc_index.(index_map v) <- !next_index) !scc;
			incr next_index;

			while !n >= 0 && visited.(index.(!n)) do
			  decr n
			done
		done;
	(ArrayUtils.of_rev_list !sccs,
 	 scc_index,
	 Dynarray.to_array (Dynarray.map [] (fun s -> IntSet.fold (fun x -> fun l -> x::l) s []) topology),
	 IntSet.fold (fun x -> fun l -> x::l) !roots []);;

end
*)