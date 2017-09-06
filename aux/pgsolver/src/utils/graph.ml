open Tcsset;;
open Tcsbasedata;;
open Tcsgraph;;
open Tcslist;;


let depth_exists start next pred =
	GraphUtils.is_reachable (start, (fun n -> Iterators.of_list (next n))) pred

let max_dist_loop_closure start prev next set =
	GraphUtils.iterate_with_maximal_distance_single_loop (start,(fun n -> Iterators.of_list (prev n)),(fun n -> Iterators.of_list (next n))) set

let min_dist_closure start next set init =
    let s = ref init in
	GraphUtils.iterate_with_minimal_distance (start, (fun n -> Iterators.of_list (next n))) (fun x i -> s := set !s x i);
	!s

let build_graph_reach_set source next =
	GraphUtils.build_reachability_set (source, (fun n -> PlainsetUtils.to_iterator (next n)))


(**************************************************************
 * Dynamic Graph                                              *
 **************************************************************)

module DynamicGraph = struct

    type 'a dynamic_graph = ((int Plainset.plainset) array) ref *
                             (('a * (int Plainset.plainset) * (int Plainset.plainset)) IntMap.t) ref *
                             int ref

    type comparison_func = int -> int -> int

    let make _ =
        (ref [||], ref IntMap.empty, ref 0)

    let add_cmp cmp (cmps, data, _) =
    	let s = IntMap.fold (fun v _ -> Plainset.add v) !data (Plainset.empty cmp) in
        cmps := Array.append !cmps [|s|];
        Array.length !cmps - 1

    let copy_graph (cmps, data, number) =
        (ref (Array.copy !cmps), ref (IntMap.fold IntMap.add !data IntMap.empty), ref !number)

    let head_copy (cmps, _, _) =
		let copy = make () in
    	Array.iter (fun s -> let _ = add_cmp (Plainset.get_compare s) copy in ()) !cmps;
    	copy

    let size (_, _, number) =
        !number

    let has_node i (_, data, _) =
        IntMap.mem i !data

    let has_edge i j (_, data, _) =
    	try
    		let (_, _, fwd) = IntMap.find i !data in
    		Plainset.mem j fwd
    	with Not_found ->
    		false

    let get_node_data v (_, data, _) =
    	let (d, _, _) = IntMap.find v !data in d

    let set_node_data v d (_, data, _) =
    	let (_, bwd, fwd) = IntMap.find v !data in
    	data := IntMap.add v (d, bwd, fwd) !data

    let get_node_pred v (_, data, _) =
    	let (_, p, _) = IntMap.find v !data in p

    let get_node_succ v (_, data, _) =
    	let (_, _, s) = IntMap.find v !data in s

    let add_node v d (cmps, data, number) =
    	if not (has_node v (cmps, data, number)) then (
    		incr number;
    		data := IntMap.add v (d, Plainset.empty compare, Plainset.empty compare) !data;
    		for i = 0 to Array.length !cmps - 1 do
    			(!cmps).(i) <- Plainset.add v (!cmps).(i)
    		done
    	)

    let del_node v (cmps, data, number) =
    	if has_node v (cmps, data, number) then (
    		decr number;
    		let (_, _, vfwd) = IntMap.find v !data in
    		Plainset.iter (fun w ->
    			let (d, wbwd, wfwd) = IntMap.find w !data in
		    	data := IntMap.add w (d, Plainset.remove v wbwd, wfwd) !data;
    		) vfwd;
    		let (_, vbwd, _) = IntMap.find v !data in
    		Plainset.iter (fun w ->
    			let (d, wbwd, wfwd) = IntMap.find w !data in
		    	data := IntMap.add w (d, wbwd, Plainset.remove v wfwd) !data;
    		) vbwd;
    		data := IntMap.remove v !data;
    		for i = 0 to Array.length !cmps - 1 do
    			(!cmps).(i) <- Plainset.remove v (!cmps).(i)
    		done
    	)

    let add_edge v w (cmps, data, number) =
    	let (vdat, vbwd, vfwd) = IntMap.find v !data in
    	data := IntMap.add v (vdat, vbwd, Plainset.add w vfwd) !data;
    	let (wdat, wbwd, wfwd) = IntMap.find w !data in
    	data := IntMap.add w (wdat, Plainset.add v wbwd, wfwd) !data

    let del_edge v w (cmps, data, number) =
		if (has_node v (cmps, data, number)) && (has_node w (cmps, data, number)) then (
    		let (vdat, vbwd, vfwd) = IntMap.find v !data in
    		data := IntMap.add v (vdat, vbwd, Plainset.remove w vfwd) !data;
    		let (wdat, wbwd, wfwd) = IntMap.find w !data in
    		data := IntMap.add w (wdat, Plainset.remove v wbwd, wfwd) !data
    	)

    let iter f (_, data, _) =
    	IntMap.iter f !data

    let iter_by f i (cmps, _, _) =
    	Plainset.iter f (!cmps).(i)

    let sub_graph_by_edge_pred pred (cmps, data, number) =
    	let (cmps', data', number') = head_copy (cmps, data, number) in
    	IntMap.iter (fun v (dat, _, fwd) ->
    		Plainset.iter (fun w ->
    			if pred v w then (
    				add_node v dat (cmps', data', number');
    				add_node w (get_node_data w (cmps, data, number)) (cmps', data', number');
    				add_edge v w (cmps', data', number')
    			)
    		) fwd
    	) !data;
    	(cmps', data', number')

	let sub_graph_by_node_closure v next (cmps, data, number) =
		let (cmps', data', number') = head_copy (cmps, data, number) in
		let rec helper w =
			if not (has_node w (cmps', data', number')) then (
				add_node w (get_node_data w (cmps, data, number)) (cmps', data', number');
				next w helper
			)
		in
			helper v;
			iter (fun w _ ->
				let fwd = get_node_succ w (cmps, data, number) in
				Plainset.iter (fun u ->
					if has_node u (cmps', data', number')
					then add_edge w u (cmps', data', number')
				) fwd
			) (cmps', data', number');
			(cmps', data', number')

	let depth_find v filt succ gr =
		let cache = ref (Plainset.empty compare) in
		let rec process u =
			if Plainset.mem u !cache then false else (
				cache := Plainset.add u !cache;
				Plainset.fold (fun w r -> r || (filt w && (succ w || process w))) (get_node_succ u gr) false
			)
		in
			process v
end