open Tcsset

val depth_exists: 'a -> ('a -> 'a list) -> ('a -> bool) -> bool

val max_dist_loop_closure: 'a -> ('a -> 'a list) -> ('a -> 'a list) -> ('a -> int -> unit) -> unit

val min_dist_closure: 'a -> ('a -> 'a list) -> ('b -> 'a -> int -> 'b) -> 'b -> 'b

val build_graph_reach_set: 'a -> ('a -> 'a Plainset.plainset) -> 'a Plainset.plainset


(**************************************************************
 * Dynamic Graph                                              *
 **************************************************************)

module DynamicGraph : sig

    type 'a dynamic_graph

    type comparison_func = int -> int -> int

    val make: unit -> 'a dynamic_graph

    val add_cmp: comparison_func -> 'a dynamic_graph -> int

    val copy_graph: 'a dynamic_graph -> 'a dynamic_graph

    val head_copy: 'a dynamic_graph -> 'a dynamic_graph

    val size: 'a dynamic_graph -> int

    val has_node: int -> 'a dynamic_graph -> bool

    val has_edge: int -> int -> 'a dynamic_graph -> bool

    val get_node_data: int -> 'a dynamic_graph -> 'a

    val set_node_data: int -> 'a -> 'a dynamic_graph -> unit

	val get_node_pred: int -> 'a dynamic_graph -> int Plainset.plainset

	val get_node_succ: int -> 'a dynamic_graph -> int Plainset.plainset

    val add_node: int -> 'a -> 'a dynamic_graph -> unit

    val del_node: int -> 'a dynamic_graph -> unit

    val add_edge: int -> int -> 'a dynamic_graph -> unit

    val del_edge: int -> int -> 'a dynamic_graph -> unit

    val iter: (int -> ('a * int Plainset.plainset * int Plainset.plainset) -> unit) -> 'a dynamic_graph -> unit

    val iter_by: (int -> unit) -> int -> 'a dynamic_graph -> unit
    
    val sub_graph_by_edge_pred: (int -> int -> bool) -> 'a dynamic_graph -> 'a dynamic_graph

	val sub_graph_by_node_closure: int -> (int -> (int -> unit) -> unit) -> 'a dynamic_graph -> 'a dynamic_graph

	val depth_find: int -> (int -> bool) -> (int -> bool)-> 'a dynamic_graph -> bool

end
