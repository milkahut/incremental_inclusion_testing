open Tcsset;;
open Tcsarray;;

module HashUtils = struct

	let hash_function f =
		let ht = Hashtbl.create 10 in
		let rec g x =
			try
				Hashtbl.find ht x
			with
				Not_found -> (
					let y = f g x in
					Hashtbl.add ht x y;
					y
				)
		in
			(g, ht)
			
	let hash_function_plain f =
		fst (hash_function f)
			
	let hash_function_with_access f =
		let ht = Hashtbl.create 10 in
		let rec g x =
			try
				let (y, n) = Hashtbl.find ht x in
				Hashtbl.replace ht x (y, n + 1);
				y
			with
				Not_found -> (
					let y = f g x in
					Hashtbl.add ht x (y, 1);
					y
				)
		in
			(g, ht)

end;;


module OrderedTypeLinearizer = struct

	type 'a node = (int * int * int) * 'a nodex
	and  'a nodex = Empty
			 	  | Branch of 'a * 'a node * 'a node

	type 'a assign_func = 'a -> int -> unit
	type 'a compare_func = 'a -> 'a -> int
	type 'a map_func = 'a -> int
			
	type 'a t = ('a assign_func * 'a map_func * 'a compare_func) * (('a node) ref)

	let new_linearizer assign map compare =
		((assign, map, compare),
		 ref ((Pervasives.min_int / 2, Pervasives.max_int / 2, 0), Empty))
		
	let space ((l,r,k), _) = r-l+1-k

	let linearize t =
		let rec helper l = function
			(_, Empty) -> l
		|	(_, Branch (x, left, right)) -> helper (x::helper l right) left
		in
			helper [] t
	
	let make l r li assign =
		let a = Array.of_list li in
		let len = Array.length a in
		if len > r - l + 1 then failwith "not enough space";
		let rec mk l r i j =
			if i > j then ((l,r,0),Empty)
			else let m = (l + r) / 2 in
				 let k = (i + j) / 2 in
				 assign a.(k) m;
				 ((l,r,j-i+1), Branch (a.(k), mk l (m - 1) i (k - 1),
											  mk (m + 1) r (k + 1) j))
		in
			mk l r 0 (len - 1)

	let rec careless_add x ((l,r,k), n) compare =
		match n with
			Empty -> ((l,r,k+1), Branch (x, ((l,r,0), Empty), ((l,r,0), Empty)))
		|	Branch (y, left, right) ->
				let c = compare x y in
				if c < 0
				then ((l,r,k+1), Branch (y, careless_add x left compare, right))
				else if c > 0
				then ((l,r,k+1), Branch (y, left, careless_add x right compare))
				else ((l,r,k), n)

	let add x ((assign, _, compare), t) =
		let rec add' t =
			let ((l,r,k), n) = t in
			match n with
				Empty ->
					let m = (l + r) / 2 in
					assign x m;
					((l,r,k+1), Branch (x, ((l,m-1,0), Empty), ((m+1,r,0), Empty)))
			|	Branch (y, left, right) ->
					let c = compare x y in
					if c = 0 then t
					else if (c < 0) && (space left > 0)
					then ((l,r,k+1), Branch (y, add' left, right))
					else if (c > 0) && (space right > 0)
					then ((l,r,k+1), Branch (y, left, add' right))
					else make l r (linearize (careless_add x t compare)) assign
		in
			if space !t < 1 then failwith "not enough space";
			t := add' !t
	
	let compare ((_, map, _), _) x y =
		compare (map x) (map y)
	
end;;


module TypeHash = struct

	type 'a t = ('a -> 'a -> int) * ((('a, int) Plainmap.plainmap) ref) * ('a DynArray.t)

	let new_hash compare x = (compare, ref (Plainmap.empty compare), DynArray.create x)
	
	let encode (_, ht, a) x =
		try
			Plainmap.find x !ht
		with
			Not_found -> (
				let i = DynArray.length a in
				DynArray.add a x;
				ht := Plainmap.add x i !ht;
				i
			)
	
	let decode (_, _, a) = DynArray.get a
	
	let compare (cmp, _, a) i j =
		cmp (DynArray.get a i) (DynArray.get a j)
	
	let size (_, _, a) = DynArray.length a
	
end;;


module OrderedTypeHash = struct

	type 'a t = ((int -> int -> int) -> 'a -> 'a -> int) *
	            ((('a, int) Plainmap.plainmap) ref) *
				('a DynArray.t) *
				(int DynArray.t) *
				(int OrderedTypeLinearizer.t)

	let new_hash compare x =
		let temp_cmp = ref (Pervasives.compare) in
		let elt_to_int = ref (Plainmap.empty (fun x y -> compare !temp_cmp x y)) in
		let int_to_elt = DynArray.create x in
		let int_to_sort = DynArray.create 0 in
		let assign = DynArray.set int_to_sort in
		let map = DynArray.get int_to_sort in
		let cmp x y = compare !temp_cmp (DynArray.get int_to_elt x)
		                                (DynArray.get int_to_elt y) in
        let lin = OrderedTypeLinearizer.new_linearizer assign map cmp in
		temp_cmp := OrderedTypeLinearizer.compare lin;
		(compare, elt_to_int, int_to_elt, int_to_sort, lin)
	
	let encode (_, ht, a, b, l) x =
		try
			Plainmap.find x !ht
		with
			Not_found -> (
				let i = DynArray.length a in
				DynArray.add a x;
				ht := Plainmap.add x i !ht;
				DynArray.add b 0;
				OrderedTypeLinearizer.add i l;
				i
			)
	
	let decode (_, _, a, _, _) = DynArray.get a
	
	let compare (_, _, _, _, lin) = OrderedTypeLinearizer.compare lin
	
	let size (_, _, a, _, _) = DynArray.length a
	
end;;
