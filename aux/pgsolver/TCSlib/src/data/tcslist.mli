module ListUtils : sig

	val custom_format: ('a -> string) -> string -> string -> string -> 'a list -> string

	val format: ('a -> string) -> 'a list -> string
			
	val format_plain: ('a -> string) -> 'a list -> string
	
	val init: int -> (int -> 'a) -> 'a list
	
	val make: int -> 'a -> 'a list
	
	val compare_lists: ('a -> 'a -> int) -> 'a list -> 'a list -> int
	
	val intersperse: 'a -> 'a list -> 'a list
	
	val min_elt: ('a -> 'a -> int) -> 'a list -> 'a
	
	val max_elt: ('a -> 'a -> int) -> 'a list -> 'a
	
	val iteri: (int -> 'a -> unit) -> 'a list -> unit
	
	val filter_map: ('a -> 'b option) -> 'a list -> 'b list
	
end


module IntListUtils : sig

	val custom_format: string -> string -> string -> int list -> string

	val format: int list -> string
			
	val format_plain: int list -> string

end

(*
type 'a iterator_type = unit -> ((unit -> 'a) *  (* next element *)
                                 (unit -> bool)) (* are there more elements? *)

val iterate_iterator: 'a iterator_type -> ('a -> unit) -> unit

val get_iterator: 'a -> ('a -> 'a) -> 'a -> 'a iterator_type

type 'a indexer_type =
	int *       (* highest index plus 1 *)
	('a -> int) (* maps item to index *)
	
val build_indexer: 'a iterator_type -> 'a indexer_type -> 'a array

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

module MakeDoublyLinkedList: functor (Init: TypeBox) ->
                             functor (Options: DoublyLinkedListOptions) ->
							 DoublyLinkedList with type item_type = Init.t
*)