module SimpleTiming = struct
	type timing_state = InactiveTimingObject | ActiveTimingObject
	type timing_object' = {
		mutable run_count: int;
		mutable call_stack: int;
		mutable total_time: float;
		mutable start_time: float;
		mutable start_hook: (timing_object -> unit) list;
		mutable stop_hook: (timing_object -> unit) list;
	}
	and timing_object = timing_object' ref
	
	let init start =
		ref {
			run_count = if start then 1 else 0;
			call_stack = if start then 1 else 0;
			total_time = 0.0;
			start_time = if start then Sys.time () else 0.0;
			start_hook = [];
			stop_hook = [];
		}
		
	let start obj =
		List.iter (fun h -> h obj) !obj.start_hook;
		let c = !obj.call_stack in
		!obj.call_stack <- c + 1;
		if c = 0 then (
			!obj.run_count <- !obj.run_count + 1;
			!obj.start_time <- Sys.time ();
		)
		
	let stop obj =
		let c = !obj.call_stack in
		!obj.call_stack <- c - 1;
		if c = 1 then (
			!obj.total_time <- !obj.total_time +. Sys.time () -. !obj.start_time
		);
		List.iter (fun h -> h obj) !obj.stop_hook
		
	let state obj =
		if !obj.call_stack > 0 then ActiveTimingObject else InactiveTimingObject
		
	let read obj =
		if !obj.call_stack > 0
		then !obj.total_time +. Sys.time () -. !obj.start_time
		else !obj.total_time
		
	let run_count obj =
		!obj.run_count
		
	let format obj =
		(Printf.sprintf "%.2f" (read obj)) ^ " sec"

	let read_avg obj =
		read obj /. float (run_count obj)
		
	let format_avg obj =
		(Printf.sprintf "%.2f" (read_avg obj)) ^ " sec"

	let register_start_hook obj hook =
		!obj.start_hook <- hook::!obj.start_hook

	let register_stop_hook obj hook =
		!obj.stop_hook <- hook::!obj.stop_hook
		
	let increment_by obj incr =
		!obj.run_count <- !obj.run_count + !incr.run_count;
		!obj.total_time <- !obj.total_time +. !incr.total_time

end;;


module HierarchicalTiming = struct
	type hierarchical_timing_object' = {
		timing_obj: SimpleTiming.timing_object;
		parent: hierarchical_timing_object;
		mutable children: hierarchical_timing_object list;
	}
	and hierarchical_timing_object = hierarchical_timing_object' ref
	
	let rec main = ref {
		timing_obj = SimpleTiming.init false;
		parent = main;
		children = []
	}
	
	let new_object parent =
		let obj = ref {
			timing_obj = SimpleTiming.init false;
			parent = parent;
			children = []
		}
		in
			!parent.children <- obj::!parent.children;
			SimpleTiming.register_start_hook !obj.timing_obj (fun _ ->
				SimpleTiming.start !parent.timing_obj
			);
			SimpleTiming.register_stop_hook !obj.timing_obj (fun _ ->
				SimpleTiming.stop !parent.timing_obj
			);
			obj
	
	let new_root_object _ = ref {
		timing_obj = SimpleTiming.init false;
		parent = main;
		children = []
	}
	
	let is_root_object obj =
		obj == main
	
	let dispose_object obj =
		if not (is_root_object obj) then (
			let p = !obj.parent in
			!p.children <- List.filter (fun c -> c!=obj) !p.children
		)
	
	let rec start obj =
		SimpleTiming.start !obj.timing_obj
	
	let rec stop obj =
		SimpleTiming.stop !obj.timing_obj
	
	let state obj = SimpleTiming.state !obj.timing_obj
	
	let read obj = SimpleTiming.read !obj.timing_obj
	
	let run_count obj = SimpleTiming.run_count !obj.timing_obj
	
	let format obj = SimpleTiming.format !obj.timing_obj

	let as_timing_object obj = !obj.timing_obj
	
end;;