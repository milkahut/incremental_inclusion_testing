open Tcstiming;;
open Tcsarray;;
open Tcsset;;
open Tcsbasedata;;

	
module NonDetInfWordAut = struct

	type ('s, 'r, 'a) automaton =
		('s, 'r, 'a) BaseType.t3 *
		's * ('s -> 'r -> 's Iterators.iterator) * ('s -> 'a)
		
	type ('s, 'r) nba = ('s, 'r, bool) automaton
	
	type ('s, 'r) npa = ('s, 'r, int) automaton
	
	type 's total_state = Default of 's | Stuck of 's
	
	
	let make_total (((cmp',fmt'), rtype, atype), start, delta, accept) stuck_accept =
		let stype' =
			let cmp x y =
				match (x, y) with
					(Default s, Default t) -> cmp' s t
				|	(Stuck s, Stuck t) -> cmp' s t
				|	(Default _, Stuck _) -> 1
				|	(Stuck _, Default _) -> -1
			in
			let fmt = function
				Default s -> fmt' s
			|	Stuck s -> "STUCK[" ^ fmt' s ^ "]"
			in
			(cmp, fmt)
		in
		
		let start' = Default start in
		
		let delta' sinst rinst =
			match sinst with
				Stuck s -> Iterators.singleton (Stuck s)
			|	Default s -> Iterators.second_if_first_empty
								(Iterators.map (fun t -> Default t) (delta s rinst))
								(Iterators.singleton (Stuck s))
		in
		
		let accept' = function
			Stuck s -> stuck_accept s
		|	Default s -> accept s
		in
		
		((stype', rtype, atype), start', delta', accept')
	
	
	let nba_make_total nba = make_total nba (fun _ -> false)
	
	let npa_make_total npa = make_total npa (fun _ -> 1)
	
	
	let profile_time (types, start, delta, accept) timingobj =
		let delta' s r =			
			SimpleTiming.start timingobj;
			let it = Iterators.explicit (delta s r) in
			SimpleTiming.stop timingobj;
			it
		in
		
		let accept' s =
			SimpleTiming.start timingobj;
			let b = accept s in
			SimpleTiming.stop timingobj;
			b
		in
		
		(types, start, delta', accept')


	let change_alphabet ((sinfo, _, ainfo), start, delta, accept) mapping =
		((sinfo, BaseType.mapping_fst mapping, ainfo), start,
		 (fun x y -> delta x (BaseType.mapping_map mapping y)), accept)
	
	let change_states ((_, rinfo, ainfo), start, delta, accept) identity =		
		let f = BaseType.identity_mapfst identity in
		let g = BaseType.identity_mapsnd identity in
		((BaseType.identity_fst identity, rinfo, ainfo), (g start),
		 (fun x y -> Iterators.map g (delta (f x) y)), (fun x -> accept (f x)))
	
	let cache (types, start, delta, accept) statemap staterulemap = 
		let delta' s r = (Mapping.cache (fun (s,r) -> Iterators.explicit (delta s r)) staterulemap) (s,r) in
		let accept' s = (Mapping.cache accept statemap) s in
		(types, start, delta', accept')
	
end;;


module DetInfWordAut = struct

	type ('s, 'r, 'a) automaton =
		('s, 'r, 'a) BaseType.t3 *
		's * ('s -> 'r -> 's) * ('s -> 'a)
		
	type ('s, 'r) dba = ('s, 'r, bool) automaton
	
	type ('s, 'r) dpa = ('s, 'r, int) automaton
	
	
	let profile_time (types, start, delta, accept) timingobj =
		let delta' s r =			
			SimpleTiming.start timingobj;
			let it = delta s r in
			SimpleTiming.stop timingobj;
			it
		in
		
		let accept' s =
			SimpleTiming.start timingobj;
			let b = accept s in
			SimpleTiming.stop timingobj;
			b
		in
		
		(types, start, delta', accept')


	let change_alphabet ((sinfo, _, ainfo), start, delta, accept) mapping =
		((sinfo, BaseType.mapping_fst mapping, ainfo), start,
		 (fun x y -> delta x (BaseType.mapping_map mapping y)), accept)
	
	let change_states ((_, rinfo, ainfo), start, delta, accept) identity =		
		let f = BaseType.identity_mapfst identity in
		let g = BaseType.identity_mapsnd identity in
		((BaseType.identity_fst identity, rinfo, ainfo), (g start),
		 (fun x y -> g (delta (f x) y)), (fun x -> accept (f x)))
	
	let cache (types, start, delta, accept) statemap staterulemap = 
		let delta' s r = (Mapping.cache (fun (s,r) -> delta s r) staterulemap) (s,r) in
		let accept' s = (Mapping.cache accept statemap) s in
		(types, start, delta', accept')
	
end;;



type ('s, 'r) nba = ('s *
                     ('s -> 'r -> 's list) *
                     ('s -> bool)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

let get_timed_nba ((start, delta, final), a, b) timingobj =
	let delta_timed s r =
		SimpleTiming.start timingobj;
		let l = delta s r in
		SimpleTiming.stop timingobj;
		l
	in
	let final_timed s =
		SimpleTiming.start timingobj;
		let b = final s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, final_timed), a, b)

let get_cached_nba ((start, delta, final), (size, cmp_state', cmp_rule'), fmt) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_cached_nba: Compare functions required."
	in
	let final_cache = ref (Plainmap.empty cmp_state) in
	let final_cached x =
		try
			Plainmap.find x !final_cache
		with Not_found -> (
			let y = final x in
			final_cache := Plainmap.add x y !final_cache;
			y
		)
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = cmp_rule r1 r2 in
		if c = 0 then cmp_state s1 s2 else c
	in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_cached x y =
		try
			Plainmap.find (x, y) !delta_cache
		with Not_found -> (
			let z = delta x y in
			delta_cache := Plainmap.add (x, y) z !delta_cache;
			z
		)
	in
	((start, delta_cached, final_cached), (size, cmp_state', cmp_rule'), fmt)

let get_int_cached_nba ((start, delta, final), (size, cmp_state', cmp_rule'), (fmt_state, fmt_rule))
                       (new_state_event, new_transition_event) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_int_cached_nba: Compare functions required."
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = compare s1 s2 in
		if c = 0 then cmp_rule r1 r2 else c
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, false) in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_count = ref 0 in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, final s);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i =
		let (s, _) = DynArray.get int_to_state i in s
	in
	let final' i =
		let (_, f) = DynArray.get int_to_state i in f
	in
	let fmt_state' i = fmt_state (int_to_state' i) in
	let delta' i r =
		try
			Plainmap.find (i, r) !delta_cache
		with Not_found -> (
			let s = int_to_state' i in
			let l = List.map state_to_int' (delta s r) in
			delta_cache := Plainmap.add (i, r) l !delta_cache;
			incr delta_count;
			new_transition_event (s, r) !delta_count;
			l
		)
	in
	((state_to_int' start, delta', final'),
	 (size, Some compare, cmp_rule'),
	 (fmt_state', fmt_rule))

let nba_switch_final ((init, delta, final), x, y) =
	let final' f = not (final f) in
	((init, delta, final'), x, y)
					 
type ('s, 'r) dba = ('s *
                     ('s -> 'r -> 's) *
                     ('s -> bool)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

let get_timed_dba ((start, delta, final), a, b) timingobj =
	let delta_timed s r =
		SimpleTiming.start timingobj;
		let l = delta s r in
		SimpleTiming.stop timingobj;
		l
	in
	let final_timed s =
		SimpleTiming.start timingobj;
		let b = final s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, final_timed), a, b)

let get_cached_dba ((start, delta, final), (index, cmp_state', cmp_rule'), fmt) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_cached_dba: Compare functions required."
	in
	let final_cache = ref (Plainmap.empty cmp_state) in
	let final_cached x =
		try
			Plainmap.find x !final_cache
		with Not_found -> (
			let y = final x in
			final_cache := Plainmap.add x y !final_cache;
			y
		)
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = cmp_rule r1 r2 in
		if c = 0 then cmp_state s1 s2 else c
	in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_cached x y =
		try
			Plainmap.find (x, y) !delta_cache
		with Not_found -> (
			let z = delta x y in
			delta_cache := Plainmap.add (x, y) z !delta_cache;
			z
		)
	in
	((start, delta_cached, final_cached), (index, cmp_state', cmp_rule'), fmt)

let get_int_cached_dba ((start, delta, final), (index, cmp_state', cmp_rule'), (fmt_state, fmt_rule))
                       (new_state_event, new_transition_event) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_int_cached_nba: Compare functions required."
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = compare s1 s2 in
		if c = 0 then cmp_rule r1 r2 else c
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, false) in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_count = ref 0 in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, final s);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i =
		let (s, _) = DynArray.get int_to_state i in s
	in
	let final' i =
		let (_, f) = DynArray.get int_to_state i in f
	in
	let fmt_state' i = fmt_state (int_to_state' i) in
	let delta' i r =
		try
			Plainmap.find (i, r) !delta_cache
		with Not_found -> (
			let s = int_to_state' i in
			let l = state_to_int' (delta s r) in
			delta_cache := Plainmap.add (i, r) l !delta_cache;
			incr delta_count;
			new_transition_event (s, r) !delta_count;
			l
		)
	in
	((state_to_int' start, delta', final'),
	 (index, Some compare, cmp_rule'),
	 (fmt_state', fmt_rule))


type ('s, 'r) npa = ('s *
                     ('s -> 'r -> 's list) *
                     ('s -> int)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

let get_timed_npa ((start, delta, omega), a, b) timingobj =
	let delta_timed s r =
		SimpleTiming.start timingobj;
		let l = delta s r in
		SimpleTiming.stop timingobj;
		l
	in
	let omega_timed s =
		SimpleTiming.start timingobj;
		let b = omega s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, omega_timed), a, b)

let get_cached_npa ((start, delta, omega), (size, cmp_state', cmp_rule'), fmt) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_cached_npa: Compare functions required."
	in
	let omega_cache = ref (Plainmap.empty cmp_state) in
	let omega_cached x =
		try
			Plainmap.find x !omega_cache
		with Not_found -> (
			let y = omega x in
			omega_cache := Plainmap.add x y !omega_cache;
			y
		)
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = cmp_rule r1 r2 in
		if c = 0 then cmp_state s1 s2 else c
	in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_cached x y =
		try
			Plainmap.find (x, y) !delta_cache
		with Not_found -> (
			let z = delta x y in
			delta_cache := Plainmap.add (x, y) z !delta_cache;
			z
		)
	in
	((start, delta_cached, omega_cached), (size, cmp_state', cmp_rule'), fmt)

let get_int_cached_npa ((start, delta, omega), (size, cmp_state', cmp_rule'), (fmt_state, fmt_rule))
                       (new_state_event, new_transition_event) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_int_cached_nba: Compare functions required."
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = compare s1 s2 in
		if c = 0 then cmp_rule r1 r2 else c
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, 0) in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_count = ref 0 in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, omega s);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i =
		let (s, _) = DynArray.get int_to_state i in s
	in
	let omega' i =
		let (_, f) = DynArray.get int_to_state i in f
	in
	let fmt_state' i = fmt_state (int_to_state' i) in
	let delta' i r =
		try
			Plainmap.find (i, r) !delta_cache
		with Not_found -> (
			let s = int_to_state' i in
			let l = List.map state_to_int' (delta s r) in
			delta_cache := Plainmap.add (i, r) l !delta_cache;
			incr delta_count;
			new_transition_event (s, r) !delta_count;
			l
		)
	in
	((state_to_int' start, delta', omega'),
	 (size, Some compare, cmp_rule'),
	 (fmt_state', fmt_rule))

type ('s, 'r) dpa = ('s *
                     ('s -> 'r -> 's) *
                     ('s -> int)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

let trivial_dpa_to_npa ((init, delta, omega), x, y) =
	let delta' s r = [delta s r] in
	((init, delta', omega), x, y)

let trivial_dba_to_dpa ((init, delta, final), (_, cs, cr), r) for_true for_false =
	let omega s = if final s then for_true else for_false in
	((init, delta, omega), (None, cs, cr), r)

let get_timed_dpa ((start, delta, omega), a, b) timingobj =
	let delta_timed s r =
		SimpleTiming.start timingobj;
		let l = delta s r in
		SimpleTiming.stop timingobj;
		l
	in
	let omega_timed s =
		SimpleTiming.start timingobj;
		let b = omega s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, omega_timed), a, b)

let get_cached_dpa ((start, delta, omega), (index, cmp_state', cmp_rule'), fmt) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_cached_dpa: Compare functions required."
	in
	let omega_cache = ref (Plainmap.empty cmp_state) in
	let omega_cached x =
		try
			Plainmap.find x !omega_cache
		with Not_found -> (
			let y = omega x in
			omega_cache := Plainmap.add x y !omega_cache;
			y
		)
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = cmp_rule r1 r2 in
		if c = 0 then cmp_state s1 s2 else c
	in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_cached x y =
		try
			Plainmap.find (x, y) !delta_cache
		with Not_found -> (
			let z = delta x y in
			delta_cache := Plainmap.add (x, y) z !delta_cache;
			z
		)
	in
	((start, delta_cached, omega_cached), (index, cmp_state', cmp_rule'), fmt)

let get_int_cached_dpa ((start, delta, omega), (index, cmp_state', cmp_rule'), (fmt_state, fmt_rule))
                       (new_state_event, new_transition_event) =
	let (cmp_state, cmp_rule) = match (cmp_state', cmp_rule') with
		(Some s, Some r) -> (s, r)
	|	_ -> failwith "get_int_cached_nba: Compare functions required."
	in
	let cmp_rule_state (s1, r1) (s2, r2) =
		let c = compare s1 s2 in
		if c = 0 then cmp_rule r1 r2 else c
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, -1) in
	let delta_cache = ref (Plainmap.empty cmp_rule_state) in
	let delta_count = ref 0 in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, omega s);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i =
		let (s, _) = DynArray.get int_to_state i in s
	in
	let omega' i =
		let (_, f) = DynArray.get int_to_state i in f
	in
	let fmt_state' i = fmt_state (int_to_state' i) in
	let delta' i r =
		try
			Plainmap.find (i, r) !delta_cache
		with Not_found -> (
			let s = int_to_state' i in
			let l = state_to_int' (delta s r) in
			delta_cache := Plainmap.add (i, r) l !delta_cache;
			incr delta_count;
			new_transition_event (s, r) !delta_count;
			l
		)
	in
	((state_to_int' start, delta', omega'),
	 (index, Some compare, cmp_rule'),
	 (fmt_state', fmt_rule))


type 's initpg = ('s *
                  ('s -> 's list) *
                  ('s -> int) *
                  ('s -> bool) *
                  ('s -> string)) *
                 (int option *
                  int option *
                  ('s -> 's -> int) option)

type 's initpg_solution = 's -> bool option

type 's initpg_strategy = 's -> 's option				  
				  
				  
				  (*
let get_compact_initpg (((start, delta, omega, player, fmt), (_, index, cmp)): 's initpg) by_player =
	let start' = (start, 0) in
	let omega' (_, p) = p in
	let player' (q, _) = player q in
	let fmt' (q, p) = fmt q ^ " " ^ string_of_int p in
	let size' = None in
	let index' = index in
	let cmp' = match cmp with
		Some c -> Some (fun (x, px) (y, py) ->
							let res = c x y in
							if res = 0 then compare px py else res)
	|	None -> None
	in
	
	let delta_p (s, p) =
		let l = delta s in
		if player s = by_player
		then List.map (fun s' -> (s', max p (omega s'))) l
		else List.map (fun s' -> (s', omega s')) l
	in
	
	let compare' =
		match cmp' with
			Some c -> c
		|	None -> compare
	in
	
	let delta' s =
		let rec helper init =
			let states = ref (Plainset.empty compare') in
			let todo = ref (Plainset.singleton compare' s) in
			while (not (Plainset.is_empty !todo)) do
				let s = Plainset.min_elt !todo in
				todo := Plainset.remove s !todo;
				if not (Plainset.mem s !states) then (
					if player' s = by_player
					then List.iter (fun s' -> todo := Plainset.add s' !todo) (delta_p s)
					else states := Plainset.add s !states
				)
			done;
			Plainset.elements !states
		in
			if player' s = by_player then helper s else delta_p s
	in
	
	((start', delta', omega', player', fmt'), (size', index', cmp'));;
*)
	
let get_compact_initpg (((start, delta, omega, player, fmt), (_, index, cmp)): 's initpg) identify_state =
	let start' = (start, 0) in
	let omega' (_, p) = p in
	let player' (q, _) = player q in
	let fmt' (q, p) = fmt q ^ " " ^ string_of_int p in
	let size' = None in
	let index' = index in
	let cmp' = match cmp with
		Some c -> Some (fun (x, px) (y, py) ->
							let res = c x y in
							if res = 0 then compare px py else res)
	|	None -> None
	in
	
	let compare' =
		match cmp' with
			Some c -> c
		|	None -> compare
	in
	
	let delta_default s =
		List.map (fun s' -> (s', omega s')) (delta s)
	in
	
	let delta_acc (s, p) =
		List.map (fun s' -> (s', max p (omega s'))) (delta s)
	in
	
	let delta' (s: 's * int) =
		let helper pl init =
			let states = ref (Plainset.empty compare') in
			let todo = ref (Plainset.singleton compare' s) in
			while (not (Plainset.is_empty !todo)) do
				let s = Plainset.min_elt !todo in
				todo := Plainset.remove s !todo;
				if not (Plainset.mem s !states) then (
					if (player' s = pl) && (not (identify_state (fst s)))
					then List.iter (fun s' -> todo := Plainset.add s' !todo) (delta_acc s)
					else states := Plainset.add s !states
				)
			done;
			Plainset.elements !states
		in
			if identify_state (fst s) then delta_default (fst s) else helper (player' s) s
	in
	
	((start', delta', omega', player', fmt'), (size', index', cmp'));;

let get_compact_initpg_by_player (pg: 's initpg) by_player =
	let ((_, _, _, pl, _), _) = pg in
	let plf s = pl s != by_player in
	get_compact_initpg pg plf;;
	
let get_escaped_initpg (((start, delta, omega, player, fmt), (size, index, cmp)): 's initpg) x =

	(((start, x),
	  (fun (s, y) -> List.map (fun s' -> (s', y)) (delta s)),
	  (fun (s, _) -> omega s),
	  (fun (s, _) -> player s),
	  (fun (s, _) -> fmt s)),
	 ((size,
	   index,
	   (match cmp with Some c -> Some (fun a b -> c (fst a) (fst b)) | None -> None))));;

				  
let get_timed_initpg ((start, delta, omega, player, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let omega_timed s =
		SimpleTiming.start timingobj;
		let b = omega s in
		SimpleTiming.stop timingobj;
		b
	in
	let player_timed s =
		SimpleTiming.start timingobj;
		let b = player s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, omega_timed, player_timed, fmt), a)

let get_cached_initpg ((start, delta, omega, player, fmt), (size, index, cmp_state')) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initpg: Compare function required."
	in
	let data_cache = ref (Plainmap.empty cmp_state) in
	let touch x =
		try
			Plainmap.find x !data_cache
		with Not_found -> (
			let y = (omega x, player x, ref None) in
			data_cache := Plainmap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, _, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun x -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let omega_cached x = let (o, _, _) = touch x in o in
	let player_cached x = let (_, p, _) = touch x in p in
	((start, delta_cached, omega_cached, player_cached, fmt), (size, index, cmp_state'))

let get_int_cached_initpg ((start, delta, omega, player, fmt), (size, index, cmp_state'))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initpg: Compare function required."
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, -1, false, ref None) in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, omega s, player s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _, _) = DynArray.get int_to_state i in s in
	let omega' i = let (_, s, _, _) = DynArray.get int_to_state i in s in
	let player' i = let (_, _, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = IntSet.elements (IntSetUtils.of_list (List.map state_to_int' (delta s))) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, delta', omega', player', fmt'),
	  (size, index, Some compare)),
	 state_to_int',
	 int_to_state')


type explicit_pg = (int * int * int array * string) array

type explicit_initpg = int * explicit_pg

let build_explicit_initpg (((start, delta, omega, player, fmt), _): int initpg) current_state_event =
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun i -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let pg = (Array.init (!max_int + 1) (fun i ->
		(omega i, (if player i then 0 else 1), Array.of_list (delta i), fmt i)
	): explicit_pg) in
	(start, pg)

let print_explicit_pg_plain game printer =
	Array.iteri (fun i (pr, pl, delta, desc) ->
		if pr >= 0 && pl >= 0 && pl <= 1 then (
			printer (string_of_int i ^ " " ^ string_of_int pr ^ " " ^ string_of_int pl ^ " ");
			printer (String.concat "," (Array.to_list (Array.map string_of_int delta)));
            if desc <> "" then printer (" \"" ^ desc ^ "\"");
            printer ";\n"
        )
	) game

let print_explicit_pg pg printer =
	printer ("parity " ^ string_of_int (Array.length pg-1) ^ ";\n");
	print_explicit_pg_plain pg printer

let print_explicit_initpg (i, pg) printer =
	printer ("parity " ^ string_of_int (Array.length pg-1) ^ ";\n");
	printer ("start " ^ string_of_int i ^ ";\n");
	print_explicit_pg_plain pg printer

type explicit_pg_solution = int array

type explicit_pg_strategy = int array

let print_explicit_pg_solution_strategy sol strat printer =
	let n = Array.length sol in
	printer ("paritysol " ^ string_of_int (n-1) ^ ";\n");
	for i = 0 to n - 1 do
		if sol.(i) >= 0 then (
            printer (string_of_int i ^ " " ^ string_of_int sol.(i));
            if strat.(i) >= 0
			then printer (" " ^ string_of_int strat.(i));
			printer ";\n"
        )
    done


type ('s, 'l, 'p) initlts =
	('s *
	 ('s -> 'p list) *
	 ('s -> ('l * 's) list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('l -> string) *
	 ('p -> string))

let get_timed_initlts ((start, props, delta, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let props_timed s =
		SimpleTiming.start timingobj;
		let b = props s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, props_timed, delta_timed, fmt), a)

let get_cached_initlts ((start, props, delta, fmt), (cmp_state', fmt_lbl, fmt_prp)) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initlts: Compare function required."
	in
	let data_cache = ref (Plainmap.empty cmp_state) in
	let touch x =
		try
			Plainmap.find x !data_cache
		with Not_found -> (
			let y = (props x, ref None) in
			data_cache := Plainmap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun (_, x) -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let props_cached x = let (o, _) = touch x in o in
	((start, props_cached, delta_cached, fmt), (cmp_state', fmt_lbl, fmt_prp))

let get_int_cached_initlts ((start, props, delta, fmt), (cmp_state', fmt_lbl, fmt_prp))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initlts: Compare function required."
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, [], ref None) in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, props s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _) = DynArray.get int_to_state i in s in
	let props' i = let (_, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = List.map (fun (l, x) -> (l, state_to_int' x)) (delta s) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, props', delta', fmt'),
	  (Some compare, fmt_lbl, fmt_prp)),
	 state_to_int',
	 int_to_state')

type explicit_lts =
	string array * (* propositions *)
	string array * (* labels *)
	(int array * (* propositions *)
	 (int * int) array * (* label,node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initlts = int * explicit_lts

let build_explicit_initlts (((start, props, delta, _), (_, fmt_lbl, fmt_prp)): (int, 'l, 'p) initlts)
                           (int_to_state: (int -> 's))
						   (fmt: ('s -> string option))
						   (current_state_event: (int -> unit)) =
	let prop_to_int = ref (Plainmap.empty compare) in
	let prop_count = ref 0 in
	let label_to_int = ref (Plainmap.empty compare) in
	let label_count = ref 0 in
	let map_prop p =
		try
			Plainmap.find p !prop_to_int
		with Not_found -> (
			let i = !prop_count in
			prop_to_int := Plainmap.add p i !prop_to_int;
			incr prop_count;
			i
		)
	in
	let map_label p =
		try
			Plainmap.find p !label_to_int
		with Not_found -> (
			let i = !label_count in
			label_to_int := Plainmap.add p i !label_to_int;
			incr label_count;
			i
		)
	in
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun (_, i) -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let graph = Array.init (!max_int + 1) (fun i ->
		(Array.of_list (List.map map_prop (props i)),
		 Array.of_list (List.map (fun (l,j) -> (map_label l, j)) (delta i)),
		 fmt (int_to_state i),
		 true)
	) in
	let proparr = Array.make !prop_count "" in
	Plainmap.iter (fun s i -> proparr.(i) <- fmt_prp s) !prop_to_int;
	let labelarr = Array.make !label_count "" in
	Plainmap.iter (fun s i -> labelarr.(i) <- fmt_lbl s) !label_to_int;
	(start, (proparr, labelarr, graph))

let print_explicit_lts_plain (props, labels, data) printer =
	let n = Array.length data in
	for i = 0 to n - 1 do
		let (nodeprops, nodetrans, nodeann, valid) = data.(i) in
		if valid then (
			printer (string_of_int i ^ " ");
			printer (String.concat "," (List.map (fun (l,j) -> labels.(l) ^ ":" ^ string_of_int j) (Array.to_list nodetrans)));
			printer " ";
			printer (String.concat "," (List.map (fun j -> props.(j)) (Array.to_list nodeprops)));
            (match nodeann with
				None -> ()
            |   Some s -> if s <> "" then printer (" \"" ^ s ^ "\""));
            printer ";\n"
		)
	done

let print_explicit_lts (props, labels, data) printer =
	printer ("lts " ^ string_of_int (Array.length data-1) ^ ";\n");
	print_explicit_lts_plain (props, labels, data) printer

let print_explicit_initlts (init, (props, labels, data)) printer =
	printer ("lts " ^ string_of_int (Array.length data-1) ^ ";\n");
	printer ("start " ^ string_of_int init ^ ";\n");
	print_explicit_lts_plain (props, labels, data) printer

type ('s, 'p) initts =
	('s *
	 ('s -> 'p list) *
	 ('s -> 's list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('p -> string))

let get_timed_initts ((start, props, delta, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let props_timed s =
		SimpleTiming.start timingobj;
		let b = props s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, props_timed, delta_timed, fmt), a)

let get_cached_initts ((start, props, delta, fmt), (cmp_state', fmt_prp)) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initlts: Compare function required."
	in
	let data_cache = ref (Plainmap.empty cmp_state) in
	let touch x =
		try
			Plainmap.find x !data_cache
		with Not_found -> (
			let y = (props x, ref None) in
			data_cache := Plainmap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun x -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let props_cached x = let (o, _) = touch x in o in
	((start, props_cached, delta_cached, fmt), (cmp_state', fmt_prp))

let get_int_cached_initts ((start, props, delta, fmt), (cmp_state', fmt_prp))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initlts: Compare function required."
	in
	let state_to_int = ref (Plainmap.empty cmp_state) in
	let int_to_state = DynArray.create (start, [], ref None) in
	let state_to_int' s =
		try
			Plainmap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := Plainmap.add s i !state_to_int;
			DynArray.add int_to_state (s, props s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _) = DynArray.get int_to_state i in s in
	let props' i = let (_, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = List.map (fun x -> state_to_int' x) (delta s) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, props', delta', fmt'),
	  (Some compare, fmt_prp)),
	 state_to_int',
	 int_to_state')

type explicit_ts =
	string array * (* propositions *)
	(int array * (* propositions *)
	 int array * (* node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initts = int * explicit_ts

let build_explicit_initts (((start, props, delta, _), (_, fmt_prp)): (int, 'p) initts)
                           (int_to_state: (int -> 's))
						   (fmt: ('s -> string option))
						   (current_state_event: (int -> unit)) =
	let prop_to_int = ref (Plainmap.empty compare) in
	let prop_count = ref 0 in
	let map_prop p =
		try
			Plainmap.find p !prop_to_int
		with Not_found -> (
			let i = !prop_count in
			prop_to_int := Plainmap.add p i !prop_to_int;
			incr prop_count;
			i
		)
	in
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun i -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let graph = Array.init (!max_int + 1) (fun i ->
		(Array.of_list (List.map map_prop (props i)),
		 Array.of_list (delta i),
		 fmt (int_to_state i),
		 true)
	) in
	let proparr = Array.make !prop_count "" in
	Plainmap.iter (fun s i -> proparr.(i) <- fmt_prp s) !prop_to_int;
	(start, (proparr, graph))

let print_explicit_ts_plain (props, data) printer =
	let n = Array.length data in
	for i = 0 to n - 1 do
		let (nodeprops, nodetrans, nodeann, valid) = data.(i) in
		if valid then (
			printer (string_of_int i ^ " ");
			printer (String.concat "," (List.map string_of_int (Array.to_list nodetrans)));
			printer " ";
			printer (String.concat "," (List.map (fun j -> props.(j)) (Array.to_list nodeprops)));
            (match nodeann with
				None -> ()
            |   Some s -> if s <> "" then printer (" \"" ^ s ^ "\""));
            printer ";\n"
		)
	done

let print_explicit_ts (props, data) printer =
	printer ("ts " ^ string_of_int (Array.length data-1) ^ ";\n");
	print_explicit_ts_plain (props, data) printer

let print_explicit_initts (init, (props, data)) printer =
	printer ("ts " ^ string_of_int (Array.length data-1) ^ ";\n");
	printer ("start " ^ string_of_int init ^ ";\n");
	print_explicit_ts_plain (props, data) printer
