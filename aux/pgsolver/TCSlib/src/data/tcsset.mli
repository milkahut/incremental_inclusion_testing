open Tcsbasedata;;

module IntOrderedType : sig
	type t = int
	val compare : t -> t -> int 
end

module IntSet :
  sig
    type elt = IntOrderedType.t
    type t = Set.Make(IntOrderedType).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

module IntSetUtils :
  sig  
	val of_list: int list -> IntSet.t
	val of_array: int array -> IntSet.t
	val of_iterator: int Iterators.iterator -> IntSet.t
	val to_iterator: IntSet.t -> int Iterators.iterator
	val iterate_subsets: IntSet.t -> (IntSet.t -> unit) -> unit
	val format: IntSet.t -> string
	val map: (int -> int) -> IntSet.t -> IntSet.t
	val map_filter: (int -> int option) -> IntSet.t -> IntSet.t
	val sym_diff: IntSet.t -> IntSet.t -> IntSet.t
  end

module IntMap :
  sig
    type key = IntOrderedType.t
    type 'a t = 'a Map.Make(IntOrderedType).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
  
module IntMapUtils :
  sig
    val pairs: 'a IntMap.t -> (int * 'a) list
    val format: ('a -> string) -> 'a IntMap.t -> string
  end

module Int2OrderedType : sig
	type t = int * int
	val compare : t -> t -> int 
end

module Int2Set :
  sig
    type elt = Int2OrderedType.t
    type t = Set.Make(Int2OrderedType).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

module Int2SetUtils :
  sig  
	val of_list: (int * int) list -> Int2Set.t
	val of_array: (int * int) array -> Int2Set.t
	val of_iterator: (int * int) Iterators.iterator -> Int2Set.t
	val to_iterator: Int2Set.t -> (int * int) Iterators.iterator
	val iterate_subsets: Int2Set.t -> (Int2Set.t -> unit) -> unit
	val format: Int2Set.t -> string
	val map: (int * int -> int * int) -> Int2Set.t -> Int2Set.t
	val map_filter: (int * int -> (int * int) option) -> Int2Set.t -> Int2Set.t
	val sym_diff: Int2Set.t -> Int2Set.t -> Int2Set.t
  end

module StringOrderedType : sig
	type t = string
	val compare : t -> t -> int
end

module StringSet :
  sig
    type elt = StringOrderedType.t
    type t = Set.Make(StringOrderedType).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

module StringMap :
  sig
    type key = StringOrderedType.t
    type 'a t = 'a Map.Make(StringOrderedType).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end

module Plainset : sig
	type 'a plainset

	val add: 'a -> 'a plainset -> 'a plainset
	val min_elt: 'a plainset -> 'a
	val max_elt: 'a plainset -> 'a
	val split: 'a -> 'a plainset -> 'a plainset * bool * 'a plainset
	val empty: ('a -> 'a -> int) -> 'a plainset
	val is_empty: 'a plainset -> bool
	val mem: 'a -> 'a plainset -> bool
	val singleton: ('a -> 'a -> int) -> 'a -> 'a plainset
	val remove: 'a -> 'a plainset -> 'a plainset
	val union: 'a plainset -> 'a plainset -> 'a plainset
	val inter: 'a plainset -> 'a plainset -> 'a plainset
	val diff: 'a plainset -> 'a plainset -> 'a plainset
	val sym_diff: 'a plainset -> 'a plainset -> 'a plainset
	val compare: 'a plainset -> 'a plainset -> int
	val equal: 'a plainset -> 'a plainset -> bool
	val subset: 'a plainset -> 'a plainset -> bool
	val iter: ('a -> unit) -> 'a plainset-> unit
	val fold: ('a -> 'b -> 'b) -> 'a plainset-> 'b -> 'b
	val for_all: ('a -> bool) -> 'a plainset-> bool
	val exists: ('a -> bool) -> 'a plainset-> bool
	val filter: ('a -> bool) -> 'a plainset-> 'a plainset
	val partition: ('a -> bool) -> 'a plainset -> 'a plainset * 'a plainset
	val cardinal: 'a plainset -> int
	val elements: 'a plainset -> 'a list
	val set_of_list: ('a -> 'a -> int) -> 'a list -> 'a plainset
	val choose: 'a plainset -> 'a
	val copy_set: 'a plainset -> ('a -> 'a -> int) -> 'a plainset
	val get_compare: 'a plainset -> ('a -> 'a -> int)
end;;

module PlainsetUtils :
  sig
	val of_list: 'a list -> ('a -> 'a -> int) -> 'a Plainset.plainset
	val of_array: 'a array -> ('a -> 'a -> int) -> 'a Plainset.plainset
	val append_iterator: 'a Plainset.plainset -> 'a Iterators.iterator -> 'a Plainset.plainset
	val to_iterator: 'a Plainset.plainset -> 'a Iterators.iterator
	val iterate_subsets: 'a Plainset.plainset -> ('a Plainset.plainset -> unit) -> unit
	val fold_subsets: ('a Plainset.plainset -> 'b -> 'b) -> 'a Plainset.plainset -> 'b -> 'b
	val format: ('a -> string) -> 'a Plainset.plainset -> string
	val remove_list_dups : 'a list -> 'a list	
	val map: ('a -> 'a) -> 'a Plainset.plainset -> 'a Plainset.plainset
	val map_filter: ('a -> 'a option) -> 'a Plainset.plainset -> 'a Plainset.plainset
	val map2: ('b -> 'b -> int) -> ('a -> 'b) -> 'a Plainset.plainset -> 'b Plainset.plainset
	val map2_filter: ('b -> 'b -> int) -> ('a -> 'b option) -> 'a Plainset.plainset -> 'b Plainset.plainset	
	val sym_diff: 'a Plainset.plainset -> 'a Plainset.plainset -> 'a Plainset.plainset
  end

module Plainmap : sig
    type ('k, 'v) plainmap
    
    val empty : ('k -> 'k -> int) -> ('k, 'v) plainmap
    val is_empty : ('k, 'v) plainmap -> bool
    val add : 'k -> 'v -> ('k, 'v) plainmap -> ('k, 'v) plainmap
    val find : 'k -> ('k, 'v) plainmap -> 'v
    val mem : 'k -> ('k, 'v) plainmap -> bool
    val remove : 'k -> ('k, 'v) plainmap -> ('k, 'v) plainmap
    val iter : ('k -> 'v -> 'unit) -> ('k, 'v) plainmap -> unit
    val for_all : ('k -> 'v -> bool) -> ('k, 'v) plainmap -> bool
    val map : ('v -> 'w) -> ('k, 'v) plainmap -> ('k, 'w) plainmap
    val mapi : ('k -> 'v -> 'w) -> ('k, 'v) plainmap -> ('k, 'w) plainmap
    val fold : ('k -> 'v -> 'a -> 'a) -> ('k, 'v) plainmap -> 'a -> 'a
    val compare :
      ('v -> 'v -> int) -> ('k, 'v) plainmap -> ('k, 'v) plainmap -> int
    val equal :
      ('v -> 'v -> bool) -> ('k, 'v) plainmap -> ('k, 'v) plainmap -> bool
end;;

module PlainmapUtils :
  sig
	val array_to_reverse_map: 'a array -> ('a -> 'a -> int) -> ('a, int) Plainmap.plainmap
	val format: ('k * 'v -> string) -> ('k, 'v) Plainmap.plainmap -> string
  end

module AvlTree :
  sig
    val bal_const : int
    type 'a avltree = Empty | Node of 'a * int * 'a avltree * 'a avltree
    val empty : 'a avltree
    val is_empty : 'a avltree -> bool
    val singleton : 'a -> 'a avltree
    val left : 'a avltree -> 'a avltree
    val right : 'a avltree -> 'a avltree
    val min_elt : 'a avltree -> 'a
    val max_elt : 'a avltree -> 'a
    val root : 'a avltree -> 'a
    val height : 'a avltree -> int
    val height_join : 'a avltree -> 'b avltree -> int
    val create : 'a -> 'a avltree -> 'a avltree -> 'a avltree
    val balance : 'a -> 'a avltree -> 'a avltree -> 'a avltree
    val join : 'a -> 'a avltree -> 'a avltree -> 'a avltree
    val take_min : 'a avltree -> 'a * 'a avltree
    val take_max : 'a avltree -> 'a * 'a avltree
    val reroot : 'a avltree -> 'a avltree -> 'a avltree
    val take_min_iter : 'a avltree -> 'a * 'a avltree
    val take_min_iter2 : 'a avltree -> 'a option * 'a avltree
    val take_max_iter : 'a avltree -> 'a * 'a avltree
    val take_max_iter2 : 'a avltree -> 'a option * 'a avltree
    val iter : ('a -> 'b) -> 'a avltree -> unit
    val fold : ('a -> 'b -> 'b) -> 'a avltree -> 'b -> 'b
    val fold_right : ('a -> 'b -> 'b) -> 'a avltree -> 'b -> 'b
    val elements : 'a avltree -> 'a list
    val for_all : ('a -> bool) -> 'a avltree -> bool
    val exists : ('a -> bool) -> 'a avltree -> bool
    val cardinal : 'a avltree -> int
    val choose : 'a avltree -> 'a
  end

module IntervalSet :
  sig
    type 'a intervalsetfuncs =
        ('a -> 'a -> int) * ('a -> 'a) * ('a -> 'a) * ('a -> 'a -> int)
        
    type 'a intervalset

    val height : 'a intervalset -> int

    val empty : 'a intervalsetfuncs -> 'a intervalset

    val is_empty : 'a intervalset -> bool

    val mem : 'a -> 'a intervalset -> bool
    
    val min_elt : 'a intervalset -> 'a
    
    val max_elt : 'a intervalset -> 'a
    
    val add : 'a -> 'a intervalset -> 'a intervalset
      
    val insert : 'a * 'a -> 'a intervalset -> 'a intervalset

    val singleton : 'a intervalsetfuncs -> 'a -> 'a intervalset
    
    val remove : 'a -> 'a intervalset -> 'a intervalset

    val union : 'a intervalset -> 'a intervalset -> 'a intervalset

    val iter : ('a -> unit) -> 'a intervalset -> unit

    val fold : ('a -> 'b -> 'b) -> 'a intervalset -> 'b -> 'b

    val fold_right : ('a -> 'b -> 'b) -> 'a intervalset -> 'b -> 'b

    val elements : 'a intervalset -> 'a list

    val for_all : ('a -> bool) -> 'a intervalset -> bool

    val exists : ('a -> bool) -> 'a intervalset -> bool

    val filter : ('a -> bool) -> 'a intervalset -> 'a intervalset

    val cardinal : 'a intervalset -> int

    val choose : 'a intervalset -> 'a
    
    val split : 'a -> 'a intervalset -> 'a intervalset * bool * 'a intervalset

    val inter : 'a intervalset -> 'a intervalset -> 'a intervalset

    val diff : 'a intervalset -> 'a intervalset -> 'a intervalset

    val compare : 'a intervalset -> 'a intervalset -> int

    val equal : 'a intervalset -> 'a intervalset -> bool

    val subset : 'a intervalset -> 'a intervalset -> bool

    val partition : ('a -> bool) -> 'a intervalset -> 'a intervalset * 'a intervalset

    val count : ('a -> bool) -> 'a intervalset -> int

  end

module IntervalSetFuncs : sig

	val int_intervalset: int IntervalSet.intervalsetfuncs
	
end
