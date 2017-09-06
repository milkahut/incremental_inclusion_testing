module ListUtils = struct

	let custom_format formater left right del = function
		[] -> left ^ right
	|	(h::t) -> (List.fold_left (fun s i -> s ^ del ^ (formater i)) (left ^ (formater h)) t) ^ right

	let format formater = custom_format formater "[" "]" ", "
	
	let format_plain formater = custom_format formater "" "" " "
	
	let init c f =
		let rec helper acc i =
			if i < 0 then acc
			else helper ((f i)::acc) (i - 1)
		in
			helper [] (c - 1)
	
	let make c a = init c (fun _ -> a)
	
	let compare_lists cmp l1 l2 =
		let rec comp = function
			(x::xs, y::ys) -> let c = cmp x y in
			                  if c != 0 then c
			                  else comp (xs, ys)
		|	_ -> 0
		in
		let c = compare (List.length l1) (List.length l2) in
		if c != 0 then c else comp (l1, l2)

	let rec intersperse y = function
		[] -> []
	|	[x] -> [x]
	|	x::xs -> x::y::(intersperse y xs)
	
	let min_elt cmp l =
		let rec helper m = function
			[] -> m
		|	x::xs -> helper (if cmp x m >= 0 then m else x) xs
		in
		helper (List.hd l) (List.tl l)
		
	let max_elt cmp = min_elt (fun x y -> cmp y x)

	let iteri f =
		let i = ref 0 in
		List.iter (fun e ->
			f !i e;
			incr i
		)
		
	let filter_map f l =
		List.fold_right (fun x m -> 
			match f x with
				Some y -> y::m
			|	None -> m
		) l [] 
	
end;;


module IntListUtils = struct

	let custom_format = ListUtils.custom_format string_of_int
	
	let format = ListUtils.format string_of_int
	
	let format_plain = ListUtils.format_plain string_of_int

end;;


(*
type 'a iterator_type = unit -> ((unit -> 'a) *  (* next element *)
                                 (unit -> bool)) (* are there more elements? *)

let iterate_iterator it f =
	let (next, has_more) = it () in
	while has_more () do f (next ()) done;;
	
let get_iterator first next nil _ =
	let current = ref first in
	let step _ =
		let a = !current in
		current := next a;
		a
	in
	let has_more _ = !current != nil in
	(step, has_more)

type 'a indexer_type =
	int *       (* highest index plus 1 *)
	('a -> int) (* maps item to index *)
	
let build_indexer iterator (count, f) =
	let a = ref [||] in
	let start = ref true in
	iterate_iterator iterator (fun item ->
		if !start then a := Array.make count item;
		!a.(f item) <- item
	);
	!a

module type TypeBox = sig
	type t
	val default: t
end

module type DoublyLinkedListOptions = sig
	val check_consistency: bool
end

module type DoublyLinkedList = sig

	exception DoublyLinkedListException of string

	type item_type
	
	type dlinkedlist
	type dlinkedlist_item

	val nil_item: dlinkedlist_item
	val nil_list: dlinkedlist

	val is_nil_item: dlinkedlist_item -> bool
	val is_nil_list: dlinkedlist -> bool
	
	val create: unit -> dlinkedlist
	
	val get_item_count: dlinkedlist -> int
	
	val get_first_item: dlinkedlist -> dlinkedlist_item
	val get_last_item: dlinkedlist -> dlinkedlist_item
	val get_next_item: dlinkedlist_item -> dlinkedlist_item
	val get_prev_item: dlinkedlist_item -> dlinkedlist_item
	
	val get_item_data: dlinkedlist_item -> item_type
	val set_item_data: dlinkedlist_item -> item_type -> unit
	val get_item_list: dlinkedlist_item -> dlinkedlist
	
	val create_item: item_type -> dlinkedlist_item
	
	val add_item2_after: dlinkedlist_item -> dlinkedlist_item -> unit
	val add_item_after: item_type -> dlinkedlist_item -> dlinkedlist_item
	
	val add_item2_before: dlinkedlist_item -> dlinkedlist_item -> unit
	val add_item_before: item_type -> dlinkedlist_item -> dlinkedlist_item

	val add_item2_as_last: dlinkedlist -> dlinkedlist_item -> unit
	val add_item_as_last: dlinkedlist -> item_type -> dlinkedlist_item

	val add_item2_as_first: dlinkedlist -> dlinkedlist_item -> unit
	val add_item_as_first: dlinkedlist -> item_type -> dlinkedlist_item
	
	val add_item2: dlinkedlist -> dlinkedlist_item -> unit
	val add_item: dlinkedlist -> item_type -> dlinkedlist_item

	val move_item_after: dlinkedlist_item -> dlinkedlist_item -> unit
	val move_item_before: dlinkedlist_item -> dlinkedlist_item -> unit
	
	val del_item: dlinkedlist_item -> unit
	
	val iterate_items_fwd: dlinkedlist -> dlinkedlist_item iterator_type
	val iterate_items_bwd: dlinkedlist -> dlinkedlist_item iterator_type
	
	val read_item_marker: dlinkedlist_item -> int
	val pop_item_marker: dlinkedlist_item -> unit
	val push_item_marker: dlinkedlist_item -> int -> unit

	val push_item_markers: dlinkedlist_item iterator_type -> (dlinkedlist_item -> int) -> unit
	val pop_item_markers: dlinkedlist_item iterator_type -> unit
	
	val push_indexer: dlinkedlist_item iterator_type -> dlinkedlist_item indexer_type
	val pop_indexer: dlinkedlist_item iterator_type -> unit
	
	val sort: dlinkedlist -> (dlinkedlist_item -> dlinkedlist_item -> int) -> unit
end

module MakeDoublyLinkedList (Init: TypeBox) (Options: DoublyLinkedListOptions) = struct

	exception DoublyLinkedListException of string

	type item_type = Init.t

	type dlinkedlist_item' = {
		mutable parent: dlinkedlist;
		mutable item_data: item_type;
		mutable prev_item: dlinkedlist_item;
		mutable next_item: dlinkedlist_item;
		mutable marker_stack: int list;
	}
	and dlinkedlist_item = dlinkedlist_item' ref
	and dlinkedlist' = {
		mutable first: dlinkedlist_item;
		mutable last: dlinkedlist_item;
		mutable count: int;
	}
	and dlinkedlist = dlinkedlist' ref
	
	let rec nil_item = ref {
		parent = nil_list;
		item_data = Init.default;
		prev_item = nil_item;
		next_item = nil_item;
		marker_stack = [];
	}
	and nil_list = ref {
		first = nil_item;
		last = nil_item;
		count = 0;
	}
	
	let is_nil_item item = item == nil_item
	
	let is_nil_list dlist = dlist == nil_list
	
	let create _ = ref {
		first = nil_item;
		last = nil_item;
		count = 0;
	}
	
	let get_item_count dlist = (!dlist).count
	
	let get_first_item dlist = (!dlist).first
	
	let get_last_item dlist = (!dlist).last
	
	let get_next_item item = (!item).next_item
	
	let get_prev_item item = (!item).prev_item
	
	let get_item_data item = (!item).item_data
	
	let set_item_data item data = (!item).item_data <- data
	
	let get_item_list item = (!item).parent
	
	let create_item data = ref {
		parent = nil_list;
		item_data = data;
		prev_item = nil_item;
		next_item = nil_item;
		marker_stack = [];
	}
	
	let assert_raise func text =
		raise (DoublyLinkedListException ("DoubleLinkedList." ^ func ^ ": " ^ text ^ "!"))
	
	let assert_unowned func text item =
		if not (is_nil_list (!item).parent) then assert_raise func text
	
	let assert_owned func text item =
		if is_nil_list (!item).parent then assert_raise func text
	
	let assert_same_parent func text item1 item2 =
		if not ((!item1).parent == (!item2).parent) then assert_raise func text
	
	let assert_proper_list func text dlist =
		if is_nil_list dlist then assert_raise func text
	
	let internal_remove parent item =
		if is_nil_item !item.prev_item
		then !parent.first <- !item.next_item
		else !(!item.prev_item).next_item <- !item.next_item;
		if is_nil_item !item.next_item
		then !parent.last <- !item.prev_item
		else !(!item.next_item).prev_item <- !item.prev_item
	
	let internal_insert_after parent item1 item2 =
		if (!item2).next_item == nil_item then (
			(!parent).last <- item1
		)
		else (
			let n = (!item2).next_item in
			(!item1).next_item <- n;
			(!n).prev_item <- item1
		);
		!item1.prev_item <- item2;
		!item2.next_item <- item1
	
	let internal_insert_before parent item1 item2 =
		if !item2.prev_item == nil_item then (
			!parent.first <- item1
		)
		else (
			let n = !item2.prev_item in
			!item1.prev_item <- n;
			!n.next_item <- item1
		);
		!item1.next_item <- item2;
		!item2.prev_item <- item1

	let add_item2_after item1 item2 =
		if Options.check_consistency then (
			assert_unowned "add_item2_after" "First item is owned" item1;
			assert_owned "add_item2_after" "Second item is not owned" item2
		);
		let parent = !item2.parent in
		!item1.parent <- parent;
		!parent.count <- !parent.count + 1;
		internal_insert_after parent item1 item2
	
	let add_item_after data item' =
		let item = create_item data in
		add_item2_after item item';
		item

	let add_item2_before item1 item2 =
		if Options.check_consistency then (
			assert_unowned "add_item2_before" "First item is owned" item1;
			assert_owned "add_item2_before" "Second item is not owned" item2
		);
		let parent = !item2.parent in
		!item1.parent <- parent;
		!parent.count <- !parent.count + 1;
		internal_insert_before parent item1 item2
	
	let add_item_before data item' =
		let item = create_item data in
		add_item2_before item item';
		item

	let add_item2_as_last dlist item =
		if Options.check_consistency then (
			assert_unowned "add_item2_as_last" "Item is owned" item;
			assert_proper_list "add_item2_as_last" "List is nil" dlist;
		);
		!item.parent <- dlist;
		let count = !dlist.count in
		if count = 0 then (
			!dlist.count <- 1;
			!dlist.first <- item;
			!dlist.last <- item
		)
		else (
			!dlist.count <- !dlist.count + 1;
			!item.prev_item <- !dlist.last;
			!(!dlist.last).next_item <- item;
			!dlist.last <- item
		)
		
	let add_item_as_last dlist data =
		let item = create_item data in
		add_item2_as_last dlist item;
		item
	
	let add_item2_as_first dlist item =
		if Options.check_consistency then (
			assert_unowned "add_item2_as_first" "Item is owned" item;
			assert_proper_list "add_item2_as_first" "List is nil" dlist;
		);
		!item.parent <- dlist;
		let count = !dlist.count in
		if count = 0 then (
			!dlist.count <- 1;
			!dlist.first <- item;
			!dlist.last <- item
		)
		else (
			!dlist.count <- !dlist.count + 1;
			!item.next_item <- !dlist.first;
			!(!dlist.first).prev_item <- item;
			!dlist.first <- item
		)
		
	let add_item_as_first dlist data =
		let item = create_item data in
		add_item2_as_first dlist item;
		item
	
	let add_item2 = add_item2_as_last
	
	let add_item = add_item_as_last

	let move_item_after item1 item2 =
		if Options.check_consistency then (
			assert_owned "move_item_after" "First item is not owned" item1;
			assert_owned "move_item_after" "Second item is not owned" item2;
			assert_same_parent "move_item_after" "Both items have to be owned by the same list" item1 item2
		);
		let parent = !item1.parent in
		internal_remove parent item1;
		internal_insert_after parent item1 item2

	let move_item_before item1 item2 =
		if Options.check_consistency then (
			assert_owned "move_item_before " "First item is not owned" item1;
			assert_owned "move_item_before " "Second item is not owned" item2;
			assert_same_parent "move_item_before " "Both items have to be owned by the same list" item1 item2
		);
		let parent = !item1.parent in
		internal_remove parent item1;
		internal_insert_before parent item1 item2

	let del_item item =
		if Options.check_consistency then (
			assert_owned "del_item " "Item is not owned" item
		);
		let parent = !item.parent in
		internal_remove parent item;
		!item.parent <- nil_list;
		!item.next_item <- nil_item;
		!item.prev_item <- nil_item;
		!parent.count <- !parent.count - 1
		
	let iterate_items_fwd dlist = get_iterator !dlist.first (fun a -> !a.next_item) nil_item

	let iterate_items_bwd dlist = get_iterator !dlist.last (fun a -> !a.prev_item) nil_item

	let read_item_marker item = List.hd !item.marker_stack
	
	let pop_item_marker item =
		!item.marker_stack <- List.tl !item.marker_stack
		
	let push_item_marker item marker =
		!item.marker_stack <- marker::!item.marker_stack

	let push_item_markers iterator marker =
		iterate_iterator iterator (fun item -> push_item_marker item (marker item))

	let pop_item_markers iterator =
		iterate_iterator iterator pop_item_marker

	let push_indexer iterator =
		let i = ref 0 in
		iterate_iterator iterator (fun item ->
			push_item_marker item !i;
			incr i
		);
		(!i, read_item_marker)

	let pop_indexer iterator =
		pop_item_markers iterator
				
	let sort dlist compare =
		(* Sorts [item_from, item_from+1, ..., item_from + item_count - 1] and
		   returns (item_first, item_last) *)
		let rec merge_sort_from item_from item_count =
			if (item_count < 2) then (item_from, item_from) else (
				(* Recursion *)
				let half = item_count / 2 in
				let (first, mid_left) = merge_sort_from item_from half in
				let item_next = get_next_item mid_left in
				let (mid_right, last) = merge_sort_from item_next (item_count - half) in
				
				(* Merge *)
				let glider_left = ref first in
				let glider_right = ref mid_right in
				let finished = ref false in
				let start = ref true in
				let final_first = ref first in
				let final_last = ref last in
				
				while (not !finished) do
					let current_left = !glider_left in
					let current_right = !glider_right in
					let sound_ordering = compare current_left current_right <= 0 in
					finished := (current_left == mid_left) || (current_right == last);
					if !finished then (
						if not sound_ordering then final_last := mid_left
					)
					else if sound_ordering
					then glider_left := get_next_item !glider_left
					else glider_right := get_next_item !glider_right;
					if !start then (
						start := false;
						if not sound_ordering then final_first := mid_right
					);
					if not sound_ordering
					then move_item_before current_right current_left
				done;
				(!final_first, !final_last)
			)
		in
			let (first, last) = merge_sort_from (get_first_item dlist) (get_item_count dlist) in
			(!dlist).first <- first;
			(!dlist).last <- last
	
end;;

*)