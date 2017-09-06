open Tcsset;;
open Tcslist;;
open Tcsautomata;;

type 'a state = ('a list * 'a list) list * int

type 'a internal_state = ('a Plainset.plainset * 'a Plainset.plainset) list * int

let gfg_nba_to_npa (((start, delta, final), (size', cmp_state', cmp_rule), (fmt_state, fmt_rule)): ('s, 'r) nba) =
	let size = match size' with
		Some s -> s
	|	None -> Pervasives.max_int / 2
	in
	let cmp_state = match cmp_state' with
		Some cmp -> cmp
	|	None -> failwith "gfg_nba_to_dpa: Compare required."
	in
    let state_to_internal l =
    	let setof s = Plainset.set_of_list cmp_state s in
    	List.map (fun (x,y) -> (setof x, setof y)) l
    in
    let internal_to_state l =
    	let listof s = Plainset.elements s in
    	List.map (fun (x,y) -> (listof x, listof y)) l
	in
    let fmt_state' (l,i) =
    	"(" ^ ListUtils.format (fun (x, y) ->
    		"(" ^ ListUtils.format fmt_state x ^ ", " ^
    		ListUtils.format fmt_state y ^ ")"
    	) l ^ "," ^ string_of_int i ^ ")"
    in
    	
	let start' =
		let f = final start in
		([([start],(if f then [start] else []))], (if f then 0 else 1)) in

	let index = match size' with
		Some s -> Some (2 * size - 1)
	|	None -> None
	in

	let omega q = 2 * size - snd q in

	let rec cmp_state'' (l1, p1) (l2, p2) =
		let subcmp (x1, y1) (x2, y2) =
			let c = ListUtils.compare_lists cmp_state x1 x2 in
			if c != 0 then c
			else ListUtils.compare_lists cmp_state y1 y2
		in
			let c = compare p1 p2 in
			if c != 0 then c
			else ListUtils.compare_lists subcmp l1 l2
	in
	
	let delta' ((state', _): 'a state) letter =
		let em = Plainset.empty cmp_state in
		let finals a = Plainset.filter final a in
		let deltas a = Plainset.fold (fun el acc -> List.fold_left (fun acc' st -> Plainset.add st acc') acc (delta el letter)) a em in
		let fold_subs f ps b = PlainsetUtils.fold_subsets f ps b in
		let fold_non_empty_subs f ps b = fold_subs (fun sub c -> if Plainset.is_empty sub then c else f sub c) ps b in
		
		let f a acc' = fold_subs (fun sub acc -> (a,sub)::acc) (finals a) acc' in
		let f_ a = f a [] in
		let f' a = fold_non_empty_subs f a [] in
		let g a b acc' = fold_subs (fun sub acc -> (a,sub)::acc) (Plainset.union (finals a) (Plainset.inter a b)) acc' in
		let g_ a b = g a b [] in
		let g' a b = fold_non_empty_subs (fun a' -> g a' b) a [] in
		let h a acc' = fold_subs (fun sub acc -> (a,sub)::acc) a acc' in
(*		let h_ a = h a [] in *)
		let h' a = fold_non_empty_subs h a [] in
		let k (a,b) = if Plainset.cardinal a = Plainset.cardinal b then f_ (deltas a) else g_ (deltas a) (deltas b) in
		let k' (a,b) = if Plainset.cardinal a = Plainset.cardinal b then f' (deltas a) else g' (deltas a) (deltas b) in 
		
		let attach head_list attachees =
			ListUtils.filter_map (fun (a,b) ->
				if List.for_all (fun (c,d) -> Plainset.cardinal (Plainset.inter a c) = 0 || Plainset.subset a d) head_list
				then Some (head_list@[(a,b)])
				else None
			) attachees
		in
		
		let eta st =
			let st = Array.of_list st in
			let n = Array.length st in
			let a = deltas (fst st.(0)) in
			let m = Plainset.cardinal a in
			let eta' m acc =
				let arr = Array.init m (fun i ->
					if i = 0 then k st.(0)
					else if i < n then k' st.(i)
					else h' a
				) in
				let l = ref (List.map (fun x -> [x]) arr.(0)) in
				for i = 1 to m - 1 do
					l := List.flatten (List.map (fun x -> attach x arr.(i)) !l)
				done;
				!l@acc
			in
			let rec hlp i acc = if i > m then acc else hlp (i+1) (eta' i acc) in
			hlp 1 []
		in
		
		let prio st =
			let fnd = ref false in
			let ind = ref 0 in
			let st = ref st in
			while (!st <> []) && not !fnd do
				let (a,b) = List.hd !st in
				st := List.tl !st;
				fnd := Plainset.cardinal a = Plainset.cardinal b;
				incr ind
			done;
			if !fnd then 2 * !ind - 2
			else 2 * !ind - 1
		in
			
		let state = state_to_internal state' in
		List.map (fun st -> (internal_to_state st, prio st)) (eta state)			
	in

	((start', delta', omega),
	 (index, Some cmp_state'', cmp_rule),
	 (fmt_state', fmt_rule))
