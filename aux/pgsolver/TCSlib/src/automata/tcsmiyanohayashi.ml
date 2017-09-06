open Tcsset;;
open Tcslist;;
open Tcsautomata;;

type 'a state = 'a list * 'a list

let miyano_hayashi (((start, delta, final), (size, cmp_state, cmp_rule), (fmt_state, fmt_rule)): ('s, 'r) nba) =

	let start' =
		([start], if final start then [] else [start])
	in
	
	let get_size =
		match size with
			Some s -> 
				let x = 2.0 ** float s in
				let value = x *. x in
				let max_value = max_int / 8 in
				let i = if (value > float max_value) then max_value else truncate value in
				Some i
		|	None -> None
	in

	let cmps =
		match cmp_state with
			None -> compare
		|	Some cmp -> cmp
	in

	let delta' (e,o) r =
		let e' = ref (Plainset.empty cmps) in
		List.iter (fun q ->	List.iter (fun p ->	e' := Plainset.add p !e') (delta q r)) e;
		let o' = ref (Plainset.empty cmps) in
		if o = []
		then o' := Plainset.filter (fun q -> not (final q)) !e'
		else List.iter (fun q -> List.iter (fun p -> if not (final p) then o' := Plainset.add p !o') (delta q r)) o;
		(Plainset.elements !e', Plainset.elements !o')
	in

	let	final' (_, l) =
		l = []
	in

	let cmp_state' =
		match cmp_state with
			None -> None
		|	Some cmp ->
				Some (fun (e1,o1) (e2,o2) ->
					let c = ListUtils.compare_lists cmp e1 e2 in
					if c = 0 then ListUtils.compare_lists cmp o1 o2
					else c
				)
	in

	let fmt_state' (e,o) =
		"(" ^ ListUtils.format fmt_state e ^ ", " ^ (if o = [] then "<EMPTY>" else ListUtils.format fmt_state o) ^ ")"
	in

	((start', delta', final'), (get_size, cmp_state', cmp_rule), (fmt_state', fmt_rule))