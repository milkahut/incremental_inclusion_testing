open Tcslist;;
open Tcsset;;
open Tcsbasedata;;


module GraphUtils : sig
	
	type 'a undirected_root_graph = 'a * ('a -> 'a Iterators.iterator)
	
	type 'a directed_root_graph = 'a * ('a -> 'a Iterators.iterator) * ('a -> 'a Iterators.iterator)

	type 'a occurrence = 'a -> bool
	
	val is_reachable2: 'a occurrence -> 'a undirected_root_graph -> ('a -> bool) -> bool
	
	val is_reachable: 'a undirected_root_graph -> ('a -> bool) -> bool
	
	val iterate_with_minimal_distance2: 'a occurrence -> 'a undirected_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_minimal_distance: 'a undirected_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_maximal_distance_single_loop2: 'a occurrence -> 'a directed_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_maximal_distance_single_loop: 'a directed_root_graph -> ('a -> int -> unit) -> unit

	val build_reachability_set2: ('a -> 'a -> int) -> 'a undirected_root_graph -> 'a Plainset.plainset

	val build_reachability_set: 'a undirected_root_graph -> 'a Plainset.plainset

end


module Digraph : sig

	type 'a node
	
	type 'a nodes = ('a node) Plainset.plainset
	
	type 'a digraph
	
	val compare_node: 'a node -> 'a node -> int
	
	val equal_node: 'a node -> 'a node -> bool
	
	val empty_nodes: unit -> 'a nodes
	
	val create: unit -> 'a digraph

	val node_count: 'a digraph -> int
	val edge_count: 'a digraph -> int
	
	val add_node: 'a digraph -> 'a -> 'a node
	val add_empty_node: 'a digraph -> 'a node
	val del_node: 'a digraph -> 'a node -> unit
	
	val add_edge: 'a digraph -> 'a node -> 'a node -> unit
	val del_edge: 'a digraph -> 'a node -> 'a node -> unit
	
	val get_content: 'a node -> 'a
	val set_content: 'a node -> 'a -> unit
	
	val get_fwd_edges: 'a node -> 'a nodes
	val get_bwd_edges: 'a node -> 'a nodes
	
	val get_nodes: 'a digraph -> 'a nodes
	
end


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

module MakeDigraph: functor (Init: TypeBox) ->
                    functor (Options: DigraphOptions) ->
					Digraph with type node_type = Init.t
*)