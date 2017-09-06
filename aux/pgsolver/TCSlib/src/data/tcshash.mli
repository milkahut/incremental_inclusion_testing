module HashUtils :
  sig
    val hash_function :
      (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a, 'b) Hashtbl.t
    val hash_function_plain : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    val hash_function_with_access :
      (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a, 'b * int) Hashtbl.t
  end

module OrderedTypeLinearizer :
  sig
	type 'a assign_func = 'a -> int -> unit
	type 'a compare_func = 'a -> 'a -> int
	type 'a map_func = 'a -> int
	type 'a t

	val new_linearizer: 'a assign_func -> 'a map_func -> 'a compare_func -> 'a t
	val add: 'a -> 'a t -> unit			
	val compare: 'a t -> 'a -> 'a -> int
  end
  
module TypeHash :
  sig
	type 'a t
	
	val new_hash: ('a -> 'a -> int) -> 'a -> 'a t
	val encode: 'a t -> 'a -> int
	val decode: 'a t -> int -> 'a
	val compare: 'a t -> int -> int -> int
	val size: 'a t -> int
  end

module OrderedTypeHash :
  sig
	type 'a t
	
	val new_hash: ((int -> int -> int) -> 'a -> 'a -> int) -> 'a -> 'a t
	val encode: 'a t -> 'a -> int
	val decode: 'a t -> int -> 'a
	val compare: 'a t -> int -> int -> int
	val size: 'a t -> int
  end
