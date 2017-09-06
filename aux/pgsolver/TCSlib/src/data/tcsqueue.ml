open Tcsset;;

module QueueUtils = struct

	let of_list l =
		let q = Queue.create () in
		List.iter (fun i -> Queue.add i q) l;
		q
		
	let of_array a =
		let q = Queue.create () in
		Array.iter (fun i -> Queue.add i q) a;
		q
		
	let of_plainset s =
		let q = Queue.create () in
		Plainset.iter (fun i -> Queue.add i q) s;
		q

end;;


module SingleOccQueue = struct

	type 'a t = 'a Queue.t * (('a Plainset.plainset) ref)
	
	let create _ =
		(Queue.create (), ref (Plainset.empty compare))
		
	let is_empty (q, _) =
		Queue.is_empty q
		
	let take (q, s) =
		let i = Queue.take q in
		s := Plainset.remove i !s;
		i
		
	let add i (q, s) =
		if not (Plainset.mem i !s) then (
			Queue.add i q;
			s := Plainset.add i !s
		)

end;;