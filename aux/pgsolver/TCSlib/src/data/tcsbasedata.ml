module BaseType = struct

	type 'x t = ('x -> 'x -> int) * ('x -> string)
	
	type ('x, 'y) t2 = 'x t * 'y t
	
	type ('x, 'y, 'z) t3 = 'x t * 'y t * 'z t


	let compare = fst
	
	let format = snd
	
	
	type ('x, 'y) mapping = 'x t * 'y t * ('x -> 'y)
	
	let mapping_fst (x, _, _) = x
	let mapping_snd (_, x, _) = x
	let mapping_map (_, _, x) = x
	
	
	type ('x, 'y) identity = 'x t * 'y t * ('x -> 'y) * ('y -> 'x)

	let identity_fst (x, _, _, _) = x
	let identity_snd (_, x, _, _) = x
	let identity_mapfst (_, _, x, _) = x
	let identity_mapsnd (_, _, _, x) = x

end;;


module Mapping = struct

	type ('x, 'y) map = 'x -> 'y
	
	type ('x, 'y) dynmap = ('x -> 'y) * ('x -> 'y -> unit)
	
	let recursive_cache f (m, s) =
		let rec n x =
			try
				m x
			with
				Not_found -> (
					let y = f n x in
					s x y;
					y
				)
		in
			n		

	let cache f (m, s) =
		let rec n x =
			try
				m x
			with
				Not_found -> (
					let y = f x in
					s x y;
					y
				)
		in
			n		

end;;


module OptionUtils = struct

	let is_none = function
		None -> true
	|	_ -> false
	
	let is_some = function
		None -> false
	|	_ -> true

	let get_some = function
		Some x -> x
	|	_ -> raise Not_found
	
	let map_some opt f =
		match opt with
			Some x -> Some (f x)
		|	None -> None
		
	let resolve opt f n =
		match opt with
			Some x -> f x
		|	None -> n
		
end;;


module Tuple = struct

	type ('a, 'b) t = 'a * 'b
	type 'a s = ('a, 'a) t
	
	let assemble x y = (x, y)
	
	let fst (x, _) = x
	
	let snd (_, y) = y
	
	let get (x, y) i =
		if i = 0
		then x
		else y

end;;


module Triple = struct

	type ('a, 'b, 'c) t = 'a * 'b * 'c
	type 'a s = ('a, 'a, 'a) t
	
	let assemble x y z = (x, y, z)
	
	let fst (x, _, _) = x
	
	let snd (_, y, _) = y
	
	let trd (_, _, z) = z
	
	let get (x, y, z) i =
		if i = 0
		then x
		else if i = 1
		then y
		else z

end;;


module Iterators = struct

	type 'a iterator = ('a -> bool) -> unit
	
    type 'a full_iterator = ('a -> unit) -> unit

	let is_empty iterator =
		let empty = ref true in
		iterator (fun _ -> empty := false; false);
		!empty

	let iter iterator f =
		iterator (fun x ->
			f x;
			true
		)
		
	let map mp iterator f =
		iterator (fun x -> f (mp x))
		
	let exists iterator f =
		let result = ref false in
		iterator (fun x ->
			result := f x;
			not !result
		);
		!result
	
	let forall iterator f =
		let result = ref true in
		iterator (fun x ->
			result := f x;
			!result
		);
		!result
		
	let fold iterator f init =
		let a = ref init in
		iterator (fun x ->
			a := f x !a;
			true
		);
		!a
		
	let filter filt iterator =
		(fun f ->
			iterator (fun x ->
				(not (filt x)) || (f x)
			)
		)
		
	let to_list iterator =
		let l = ref [] in
		iterator (fun x ->
			l := x::!l;
			true
		);
		List.rev !l
		
	let rec of_list l f =
		match l with
			[] -> ()
		|	x::xs -> if f x then of_list xs f
		
	let explicit iterator =
		of_list (to_list iterator)
		
	let singleton x f =
		let _ = f x in ()
		
	let to_array iterator =
		Array.of_list (to_list iterator)
		
	let of_array a f =
		let n = Array.length a in
		let i = ref 0 in
		while (!i < n) && (f a.(!i)) do
			incr i
		done
		
	let of_full_iterator iter f =
		let res = ref true in
		iter (fun x -> if !res then res := f x)

	let second_if_first_empty it1 it2 f =
		let empty = ref true in
		it1 (fun x ->
			empty := false;
			f x
		);
		if !empty then it2 f
		
	let attach it1 it2 f =
		let result = ref true in
		it1 (fun x ->
			result := f x;
			!result
		);
		if !result then it2 f
		
	let flatten iterator_iterator f =
		iterator_iterator (fun sub_iterator ->
			let result = ref true in
			sub_iterator (fun x ->
				result := f x;
				!result
			);
			!result
		)		
		
	let product it1 it2 =
		(fun f ->
			let result = ref true in
			it1 (fun x ->
				if !result then (
					it2 (fun y ->
						result := f (x,y);
						!result
					)
				);
				!result
			)
		)
		
	let depend_product it1 it2 =
		(fun f ->
			let result = ref true in
			it1 (fun x ->
				if !result then (
					it2 x (fun y ->
						result := f (x,y);
						!result
					)
				);
				!result
			)
		)

end;;


module Enumerators = struct

	type 'a enumerator = Empty | Next of ('a enumerator' * 'a)
	and 'a enumerator' = unit -> 'a enumerator

	type ('a, 'b) t = 'b -> ('b * 'a) option
	
	let make factory init =
		let rec f state _ = 
			match factory state with
				None -> Empty
			|	Some (state', x) -> Next (f state', x)
		in
			f init ()
			
	let empty = function
		Empty -> true
	|	_ -> false
	
	let head = function
		Empty -> raise Not_found
	|	Next (_, x) -> x
	
	let tail = function
		Empty -> raise Not_found
	|	Next (y, _) -> y ()
	
	let next = function
		Empty -> raise Not_found
	|	Next (y, x) -> (y (), x)

	let to_iterator enum f =
		let rec g = function
			Empty -> ()
		|	Next (y, x) -> if f x then g (y ()) else ()
		in
			g enum
	
	let of_array arr =
		let l = Array.length arr in
		let f i = 
			if i >= l then None
			else Some (i+1, arr.(i))
		in
			make f 0
			
	let of_list li =
		let f = function
			[] -> None
		|	(x::xs) -> Some (xs,x)
		in
			make f li
			
	let singleton x =
		make (fun state -> OptionUtils.map_some state (fun y -> (None, y))) (Some x)
			
	let rec map f = function
		Empty -> Empty
	|	Next (y, x) -> Next ((fun un -> map f (y un)), f x)
	
	let to_list e = Iterators.to_list (to_iterator e)
end;;


module UniqueIntegerAssignment = struct

    type t = (int ref) * ((int list) ref)
	
	let new_assignment _ =
		(ref 0, ref [])
		
	let assign (limit, lst) =
		match !lst with
			[] -> (
				incr limit;
				!limit - 1
			)
		|	x::xs -> (
				lst := xs;
				x
			)

	let release (limit, lst) i =
		if i = !limit - 1
		then decr limit
		else lst := i::!lst

end;;
  
  

module CompRef = struct
	
	type 'a compref =  {
		index: int;
		mutable content : 'a;
	}

	let uia =
		UniqueIntegerAssignment.new_assignment ()
	
	let newref x = {
		index = UniqueIntegerAssignment.assign uia;
		content = x
	}
		
	let freeref x =
		UniqueIntegerAssignment.release uia x.index
		
	let getref x =
		x.content
		
	let setref x y =
		x.content <- y
		
	let compare a b =
		compare a.index b.index
		
	let equal a b =
		a == b
		
end;;
