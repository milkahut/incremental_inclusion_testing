open Tcslist;;
open Tcsgraph;;
(*
let out s = print_string s;;

module CityInfo = struct
	type t = string * int
	
	let default = ("", 0)
end


module Options = struct
	let check_consistency = true
end

module CityGraph = MakeDigraph(CityInfo)(Options);;

let g = CityGraph.create ()

let munich = CityGraph.add_node g ("Munich", 1365052)
let frankfurt = CityGraph.add_node g ("Frankfurt", 659021)
let berlin = CityGraph.add_node g ("Berlin", 3429870)
let nirvana = CityGraph.add_node g ("Nirvana", 0)
let _ = CityGraph.add_edge munich frankfurt
let _ = CityGraph.add_edge frankfurt berlin
let _ = CityGraph.add_edge berlin munich
let _ = CityGraph.add_edge berlin frankfurt
let _ = CityGraph.add_edge munich nirvana

let indexer = CityGraph.push_indexer (CityGraph.iterate_nodes_fwd g)
let (sccs, sccindex, top, roots) = CityGraph.strongly_connected_components g indexer

let _ =
	out "\nGraph Test 1\n";
	out ("Nodes: " ^ string_of_int (CityGraph.get_node_count g) ^ "\n");
	out ("Edges: " ^ string_of_int (CityGraph.get_edge_count g) ^ "\n");
	out ("Roots: " ^ string_of_int (List.length roots) ^ "\n");
	out ("Comps: " ^ string_of_int (Array.length top) ^ "\n");
	iterate_iterator (CityGraph.iterate_nodes_fwd g) (fun node ->
		let (name, cit) = CityGraph.get_node_data node in
		out (name ^ " (" ^ string_of_int cit ^ " Citizens)\n");
		out "    connections to";
		iterate_iterator (CityGraph.iterate_node_fwd_edges_fwd node) (fun edge ->
			let node' = CityGraph.get_edge_target edge in
			let (name', _) = CityGraph.get_node_data node' in
			out (" " ^ name');
		);
		out "\n";
		out ("    connections from");
		iterate_iterator (CityGraph.iterate_node_bwd_edges_fwd node) (fun edge ->
			let node' = CityGraph.get_edge_source edge in
			let (name', _) = CityGraph.get_node_data node' in
			out (" " ^ name');
		);
		out "\n";
	);
*)