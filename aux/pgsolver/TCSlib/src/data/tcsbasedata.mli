module BaseType : sig

	type 'x t = ('x -> 'x -> int) * ('x -> string)
	
	type ('x, 'y) t2 = 'x t * 'y t
	
	type ('x, 'y, 'z) t3 = 'x t * 'y t * 'z t
	
	
	val compare: 'x t -> 'x -> 'x -> int
	
	val format: 'x t -> 'x -> string
	
	
	type ('x, 'y) mapping = 'x t * 'y t * ('x -> 'y)
	
	val mapping_fst: ('x, 'y) mapping -> 'x t
	val mapping_snd: ('x, 'y) mapping -> 'y t
	val mapping_map: ('x, 'y) mapping -> ('x -> 'y)
	
	type ('x, 'y) identity = 'x t * 'y t * ('x -> 'y) * ('y -> 'x)

	val identity_fst: ('x, 'y) identity -> 'x t
	val identity_snd: ('x, 'y) identity -> 'y t
	val identity_mapfst: ('x, 'y) identity -> ('x -> 'y)
	val identity_mapsnd: ('x, 'y) identity -> ('y -> 'x)

end


module Mapping : sig

	type ('x, 'y) map = 'x -> 'y
	
	type ('x, 'y) dynmap = ('x, 'y) map * ('x -> 'y -> unit)
	
	val recursive_cache: (('x, 'y) map -> ('x, 'y) map) -> ('x, 'y) dynmap -> ('x, 'y) map

	val cache: ('x, 'y) map -> ('x, 'y) dynmap -> ('x, 'y) map

end


module OptionUtils : sig

	val is_none: 'a option -> bool
	
	val is_some: 'a option -> bool
	
	val get_some: 'a option -> 'a
	
	val map_some : 'a option -> ('a -> 'b) -> 'b option
	
	val resolve : 'a option -> ('a -> 'b) -> 'b -> 'b

end


module Tuple : sig

	type ('a, 'b) t = 'a * 'b
	type 'a s = ('a, 'a) t
	
	val assemble: 'a -> 'b -> ('a, 'b) t
	val fst: ('a, 'b) t -> 'a
	val snd: ('a, 'b) t -> 'b
	val get: 'a s -> int -> 'a

end


module Triple : sig

	type ('a, 'b, 'c) t = 'a * 'b * 'c
	type 'a s = ('a, 'a, 'a) t
	
	val assemble: 'a -> 'b -> 'c -> ('a, 'b, 'c) t
	val fst: ('a, 'b, 'c) t -> 'a
	val snd: ('a, 'b, 'c) t -> 'b
	val trd: ('a, 'b, 'c) t -> 'c
	val get: 'a s -> int -> 'a

end


module Iterators :
  sig
    type 'a iterator = ('a -> bool) -> unit
    type 'a full_iterator = ('a -> unit) -> unit
	val is_empty : 'a iterator -> bool
    val iter : 'a iterator -> 'a full_iterator
	val map: ('a -> 'b) -> 'a iterator -> 'b iterator
    val exists : 'a iterator -> ('a -> bool)  -> bool
    val forall : 'a iterator -> ('a -> bool) -> bool
    val fold : 'a iterator -> ('a -> 'c -> 'c) -> 'c -> 'c
	val filter : ('a -> bool) -> 'a iterator -> 'a iterator
    val to_list : 'a iterator -> 'a list
    val of_list : 'a list -> 'a iterator
	val explicit : 'a iterator -> 'a iterator
	val singleton: 'a -> 'a iterator
    val to_array : 'a iterator -> 'a array
    val of_array : 'a array -> 'a iterator
    val of_full_iterator: 'a full_iterator -> 'a iterator
	val second_if_first_empty: 'a iterator -> 'a iterator -> 'a iterator
	val attach: 'a iterator -> 'a iterator -> 'a iterator
	val flatten: ('a iterator) iterator -> 'a iterator
	val product: 'a iterator -> 'b iterator -> ('a * 'b) iterator
	val depend_product: 'a iterator -> ('a -> 'b iterator) -> ('a * 'b) iterator
  end

module Enumerators :
  sig
	type ('a, 'b) t = 'b -> ('b * 'a) option
	type 'a enumerator
	
	val make: ('a, 'b) t -> 'b -> 'a enumerator
	
	val empty : 'a enumerator -> bool
	val head : 'a enumerator -> 'a
	val tail : 'a enumerator -> 'a enumerator
	val next: 'a enumerator -> 'a enumerator * 'a
	val to_iterator: 'a enumerator -> 'a Iterators.iterator
	val of_array: 'a array -> 'a enumerator
	val of_list: 'a list -> 'a enumerator
	val singleton: 'a -> 'a enumerator
	val map: ('a -> 'b) -> 'a enumerator -> 'b enumerator
	val to_list: 'a enumerator -> 'a list
end


  
module UniqueIntegerAssignment :
  sig
    type t
    val new_assignment: unit -> t
	val assign: t -> int
	val release: t -> int -> unit
  end
  
  
module CompRef :
  sig
	type 'a compref
	
	val newref: 'a -> 'a compref
	val freeref: 'a compref -> unit
	val getref: 'a compref -> 'a
	val setref: 'a compref -> 'a -> unit
	val compare: 'a compref -> 'a compref -> int
	val equal: 'a compref -> 'a compref -> bool
  end
  
  