open Tcslist;;
open Tcsbasedata;;

let _set_iterate_subsets (empty, is_empty, min_elt, remove, add) ed f =
	let rec helper base var =
		if is_empty var then f base else (
			let i = min_elt var in
			let var' = remove i var in
			helper base var';
			helper (add i base) var'
		)
	in
		helper empty ed

module IntOrderedType =
struct
  type t = int
  let compare = compare
end;;

module IntSet = Set.Make(IntOrderedType);;

module IntSetUtils = struct
	let of_list l =
		List.fold_left (fun s e -> IntSet.add e s) IntSet.empty l
		
	let of_array a =
		Array.fold_left (fun s e -> IntSet.add e s) IntSet.empty a
		
	let of_iterator iter =
		let s = ref IntSet.empty in
		Iterators.iter iter (fun x -> s := IntSet.add x !s);
		!s
		
	let to_iterator s =
		Iterators.of_full_iterator (fun f -> IntSet.iter f s)

	let iterate_subsets =
		_set_iterate_subsets (IntSet.empty, IntSet.is_empty, IntSet.min_elt, IntSet.remove, IntSet.add)
	
	let format s =
		ListUtils.format string_of_int (IntSet.elements s)
		
	let map f t =
		IntSet.fold (fun x -> IntSet.add (f x)) t IntSet.empty
		
	let map_filter f t =
		IntSet.fold (fun x s -> 
			match f x with
				Some y -> IntSet.add y s
			|	None -> s
		) t IntSet.empty

	let sym_diff x y =
		IntSet.union (IntSet.diff x y) (IntSet.diff y x)

end;;

module IntMap = Map.Make(IntOrderedType) ;;

module IntMapUtils = struct

	let pairs im =
		let l = ref [] in
		IntMap.iter (fun x y ->
			l := (x,y)::!l
		) im;
		!l

	let format f im =
		ListUtils.format (fun (x,y) -> string_of_int x ^ "->" ^ f y) (pairs im)

end;;

module Int2OrderedType =
struct
  type t = int * int
  let compare = compare
end;;

module Int2Set = Set.Make(Int2OrderedType);;

module Int2SetUtils = struct
	let of_list l =
		List.fold_left (fun s e -> Int2Set.add e s) Int2Set.empty l
		
	let of_array a =
		Array.fold_left (fun s e -> Int2Set.add e s) Int2Set.empty a
		
	let of_iterator iter =
		let s = ref Int2Set.empty in
		Iterators.iter iter (fun x -> s := Int2Set.add x !s);
		!s
		
	let to_iterator s =
		Iterators.of_full_iterator (fun f -> Int2Set.iter f s)

	let iterate_subsets =
		_set_iterate_subsets (Int2Set.empty, Int2Set.is_empty, Int2Set.min_elt, Int2Set.remove, Int2Set.add)
	
	let format s =
		ListUtils.format (fun (i,j) -> "(" ^ string_of_int i ^ "," ^ string_of_int j ^ ")") (Int2Set.elements s)

	let map f t =
		Int2Set.fold (fun x -> Int2Set.add (f x)) t Int2Set.empty
		
	let map_filter f t =
		Int2Set.fold (fun x s -> 
			match f x with
				Some y -> Int2Set.add y s
			|	None -> s
		) t Int2Set.empty

	let sym_diff x y =
		Int2Set.union (Int2Set.diff x y) (Int2Set.diff y x)

end;;

module StringOrderedType =
struct
  type t = string
  let compare = compare
end;;

module StringSet = Set.Make(StringOrderedType);;

module StringMap = Map.Make(StringOrderedType) ;;

module Plainset = struct

	type 'a baltree = Empty | Node of 'a baltree * 'a * 'a baltree * int

	type 'a plainset = ('a -> 'a -> int) * 'a baltree

	let height = function
		Empty -> 0
	  | Node(_, _, _, h) -> h

	let create l v r =
	  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
	  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
	  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

	let bal l v r =
	  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
	  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
	  if hl > hr + 2 then begin
		match l with
		  Empty -> invalid_arg "Plainset.bal"
		| Node(ll, lv, lr, _) ->
			if height ll >= height lr then
			  create ll lv (create lr v r)
			else begin
			  match lr with
				Empty -> invalid_arg "Plainset.bal"
			  | Node(lrl, lrv, lrr, _)->
				  create (create ll lv lrl) lrv (create lrr v r)
			end
	  end else if hr > hl + 2 then begin
		match r with
		  Empty -> invalid_arg "Plainset.bal"
		| Node(rl, rv, rr, _) ->
			if height rr >= height rl then
			  create (create l v rl) rv rr
			else begin
			  match rl with
				Empty -> invalid_arg "Plainset.bal"
			  | Node(rll, rlv, rlr, _) ->
				  create (create l v rll) rlv (create rlr rv rr)
			end
	  end else
		Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

	let rec add' cmp x = function
		Empty -> Node(Empty, x, Empty, 1)
	  | Node(l, v, r, _) as t ->
		  let c = cmp x v in
		  if c = 0 then t else
		  if c < 0 then bal (add' cmp x l) v r else bal l v (add' cmp x r)

	let add x (cmp, t) = (cmp, add' cmp x t)

	let rec join cmp l v r =
	  match (l, r) with
		(Empty, _) -> add' cmp v r
	  | (_, Empty) -> add' cmp v l
	  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
		  if lh > rh + 2 then bal ll lv (join cmp lr v r) else
		  if rh > lh + 2 then bal (join cmp l v rl) rv rr else
		  create l v r

	let rec min_elt' = function
		Empty -> raise Not_found
	  | Node(Empty, v, r, _) -> v
	  | Node(l, v, r, _) -> min_elt' l

	let min_elt (_, t) = min_elt' t

	let rec max_elt' = function
		Empty -> raise Not_found
	  | Node(l, v, Empty, _) -> v
	  | Node(l, v, r, _) -> max_elt' r

	let max_elt (_, t) = max_elt' t

	let rec remove_min_elt = function
		Empty -> invalid_arg "Plainset.remove_min_elt"
	  | Node(Empty, v, r, _) -> r
	  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

	let merge t1 t2 =
	  match (t1, t2) with
		(Empty, t) -> t
	  | (t, Empty) -> t
	  | (_, _) -> bal t1 (min_elt' t2) (remove_min_elt t2)

	let concat cmp t1 t2 =
	  match (t1, t2) with
		(Empty, t) -> t
	  | (t, Empty) -> t
	  | (_, _) -> join cmp t1 (min_elt' t2) (remove_min_elt t2)

	let rec split' cmp x = function
		Empty ->
		  (Empty, false, Empty)
	  | Node(l, v, r, _) ->
		  let c = cmp x v in
		  if c = 0 then (l, true, r)
		  else if c < 0 then
			let (ll, pres, rl) = split' cmp x l in (ll, pres, join cmp rl v r)
		  else
			let (lr, pres, rr) = split' cmp x r in (join cmp l v lr, pres, rr)

	let split x (cmp, t) =
		let (a, r, b) = split' cmp x t in ((cmp, a), r, (cmp, b))

	let empty cmp = (cmp, Empty)

	let is_empty = function (_, Empty) -> true | _ -> false

	let rec mem x = function
		(_, Empty) -> false
	  | (cmp, Node(l, v, r, _)) ->
		  let c = cmp x v in
		  c = 0 || mem x (if c < 0 then (cmp, l) else (cmp, r))

	let singleton cmp x = (cmp, Node(Empty, x, Empty, 1))

	let rec remove' cmp x = function
		Empty -> Empty
	  | Node(l, v, r, _) ->
		  let c = cmp x v in
		  if c = 0 then merge l r else
		  if c < 0 then bal (remove' cmp x l) v r else bal l v (remove' cmp x r)

	let remove x (cmp, t) = (cmp, remove' cmp x t)

	let rec union' cmp s1 s2 =
	  match (s1, s2) with
		(Empty, t2) -> t2
	  | (t1, Empty) -> t1
	  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
		  if h1 >= h2 then
			if h2 = 1 then add' cmp v2 s1 else begin
			  let (l2, _, r2) = split' cmp v1 s2 in
			  join cmp (union' cmp l1 l2) v1 (union' cmp r1 r2)
			end
		  else
			if h1 = 1 then add' cmp v1 s2 else begin
			  let (l1, _, r1) = split' cmp v2 s1 in
			  join cmp (union' cmp l1 l2) v2 (union' cmp r1 r2)
			end

	let union (cmp, s1) (_, s2) = (cmp, union' cmp s1 s2)

	let rec inter' cmp s1 s2 =
	  match (s1, s2) with
		(Empty, t2) -> Empty
	  | (t1, Empty) -> Empty
	  | (Node(l1, v1, r1, _), t2) ->
		  match split' cmp v1 t2 with
			(l2, false, r2) ->
			  concat cmp (inter' cmp l1 l2) (inter' cmp r1 r2)
		  | (l2, true, r2) ->
			  join cmp (inter' cmp l1 l2) v1 (inter' cmp r1 r2)

	let inter (cmp, s1) (_, s2) = (cmp, inter' cmp s1 s2)

	let rec diff' cmp s1 s2 =
	  match (s1, s2) with
		(Empty, t2) -> Empty
	  | (t1, Empty) -> t1
	  | (Node(l1, v1, r1, _), t2) ->
		  match split' cmp v1 t2 with
			(l2, false, r2) ->
			  join cmp (diff' cmp l1 l2) v1 (diff' cmp r1 r2)
		  | (l2, true, r2) ->
			  concat cmp (diff' cmp l1 l2) (diff' cmp r1 r2)

	let diff (cmp, s1) (_, s2) = (cmp, diff' cmp s1 s2)

	let sym_diff' cmp s1 s2 =
		union' cmp (diff' cmp s1 s2) (diff' cmp s2 s1)

	let sym_diff (cmp, s1) (_, s2) = (cmp, sym_diff' cmp s1 s2)

	type 'a enumeration = End | More of 'a * 'a baltree * 'a enumeration

	let rec cons_enum s e =
	  match s with
		Empty -> e
	  | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

	let rec compare_aux cmp e1 e2 =
		match (e1, e2) with
		(End, End) -> 0
	  | (End, _)  -> -1
	  | (_, End) -> 1
	  | (More(v1, r1, e1), More(v2, r2, e2)) ->
		  let c = cmp v1 v2 in
		  if c <> 0
		  then c
		  else compare_aux cmp (cons_enum r1 e1) (cons_enum r2 e2)

	let compare (cmp, s1) (_, s2) =
	  compare_aux cmp (cons_enum s1 End) (cons_enum s2 End)

	let equal s1 s2 =
	  compare s1 s2 = 0

	let rec subset' cmp s1 s2 =
	  match (s1, s2) with
		Empty, _ ->
		  true
	  | _, Empty ->
		  false
	  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
		  let c = cmp v1 v2 in
		  if c = 0 then
			subset' cmp l1 l2 && subset' cmp r1 r2
		  else if c < 0 then
			subset' cmp (Node (l1, v1, Empty, 0)) l2 && subset' cmp r1 t2
		  else
			subset' cmp (Node (Empty, v1, r1, 0)) r2 && subset' cmp l1 t2

	let subset (cmp, s1) (_, s2) = subset' cmp s1 s2

	let rec iter f = function
		(_, Empty) -> ()
	  | (c, Node(l, v, r, _)) -> iter f (c, l); f v; iter f (c, r)

	let rec fold f (c, s) accu =
	  match s with
		Empty -> accu
	  | Node(l, v, r, _) -> fold f (c, r) (f v (fold f (c, l) accu))

	let rec for_all p = function
		(_, Empty) -> true
	  | (c, Node(l, v, r, _)) -> p v && for_all p (c, l) && for_all p (c, r)

	let rec exists p = function
		(_, Empty) -> false
	  | (c, Node(l, v, r, _)) -> p v || exists p (c, l) || exists p (c, r)

	let filter p (cmp, s) =
	  let rec filt accu = function
		| Empty -> accu
		| Node(l, v, r, _) ->
			filt (filt (if p v then add' cmp v accu else accu) l) r in
	  (cmp, filt Empty s)

	let partition p (cmp, s) =
	  let rec part (t, f as accu) = function
		| Empty -> accu
		| Node(l, v, r, _) ->
			part (part (if p v then (add' cmp v t, f) else (t, add' cmp v f)) l) r in
	  let (t1, t2) = part (Empty, Empty) s in
	  ((cmp, t1), (cmp, t2))

	let rec cardinal = function
		(_, Empty) -> 0
	  | (c, Node(l, v, r, _)) -> cardinal (c, l) + 1 + cardinal (c, r)

	let rec elements_aux accu = function
		Empty -> accu
	  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

	let elements (_, s) =
	  elements_aux [] s
	  
	let set_of_list cmp l =
		List.fold_left (fun s e -> add e s) (empty cmp) l

	let choose = min_elt

	let copy_set s cmp' =
		fold add s (empty cmp')
		
	let get_compare (cmp, _) = cmp
	
end;;

module PlainsetUtils = struct
	let of_list l comp =
		List.fold_left (fun s e -> Plainset.add e s) (Plainset.empty comp) l
		
	let of_array a comp =
		Array.fold_left (fun s e -> Plainset.add e s) (Plainset.empty comp) a
		
	let append_iterator s iter =
		let s = ref s in
		Iterators.iter iter (fun x -> s := Plainset.add x !s);
		!s
		
	let to_iterator s =
		Iterators.of_full_iterator (fun f -> Plainset.iter f s)

	let iterate_subsets ps it =
		let comp = fst ps in
		_set_iterate_subsets (Plainset.empty comp, Plainset.is_empty, Plainset.min_elt, Plainset.remove, Plainset.add) ps it
		
	let fold_subsets f ps s =
		let s = ref s in
		iterate_subsets ps (fun sub ->
			s := f sub !s
		);
		!s
	
	let format fmt s =
		ListUtils.format fmt (Plainset.elements s)
		
	let remove_list_dups l =
		let s = ref (Plainset.empty compare) in
		List.fold_right (fun x t ->
			if Plainset.mem x !s then t
			else (
				s := Plainset.add x !s;
				x::t
			)
		) l []

	let map f t =
		Plainset.fold (fun x -> Plainset.add (f x)) t (Plainset.empty (Plainset.get_compare t))
		
	let map_filter f t =
		Plainset.fold (fun x s -> 
			match f x with
				Some y -> Plainset.add y s
			|	None -> s
		) t (Plainset.empty (Plainset.get_compare t))

	let map2 c f t =
		Plainset.fold (fun x -> Plainset.add (f x)) t (Plainset.empty c)
		
	let map2_filter c f t =
		Plainset.fold (fun x s -> 
			match f x with
				Some y -> Plainset.add y s
			|	None -> s
		) t (Plainset.empty c)

	let sym_diff x y =
		Plainset.union (Plainset.diff x y) (Plainset.diff y x)

end;;


module Plainmap = struct

    type ('k, 'v) baltree =
        Empty
      | Node of ('k, 'v) baltree * 'k * 'v * ('k, 'v) baltree * int
      
    type ('k, 'v) plainmap = ('k -> 'k -> int) * ('k, 'v) baltree

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty cmp = (cmp, Empty)

    let is_empty' = function Empty -> true | _ -> false
    
    let is_empty (_, t) = is_empty' t

    let rec add' cmp x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = cmp x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add' cmp x data l) v d r
          else
            bal l v d (add' cmp x data r)
            
    let add x data (cmp, t) = (cmp, add' cmp x data t)

    let rec find' cmp x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = cmp x v in
          if c = 0 then d
          else find' cmp x (if c < 0 then l else r)
    
    let find x (cmp, t) = find' cmp x t

    let rec mem' cmp x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = cmp x v in
          c = 0 || mem' cmp x (if c < 0 then l else r)
    
    let mem x (cmp, t) = mem' cmp x t
    
    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l
      
    let rec remove_min_binding = function
        Empty -> invalid_arg "Plainmap.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove' cmp x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = cmp x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove' cmp x l) v d r
          else
            bal l v d (remove' cmp x r)

    let remove x (cmp, t) = (cmp, remove' cmp x t)

    let rec iter' f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter' f l; f v d; iter' f r

    let iter f (_, t) = iter' f t

	let rec for_all p = function
		(_, Empty) -> true
	  | (c, Node(l, v, d, r, _)) -> for_all p (c, l) && p v d && for_all p (c, r)

  let rec map' f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map' f l, v, f d, map' f r, h)

    let map f (cmp, t) = (cmp, map' f t)

    let rec mapi' f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(mapi' f l, v, f v d, mapi' f r, h)

    let mapi f (cmp, t) = (cmp, mapi' f t)

    let rec fold' f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold' f r (f v d (fold' f l accu))

    let fold f (_, m) accu = fold' f m accu

    type ('k, 'v) enumeration = End | More of 'k * 'v * ('k, 'v) baltree * ('k, 'v) enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp (cmpkey, m1) (_, m2) =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = cmpkey v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp (cmpkey, m1) (_, m2) =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            cmpkey v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

end

module PlainmapUtils = struct

	let array_to_reverse_map arr comp =
		let mp = ref (Plainmap.empty comp) in
		Array.iteri (fun i x -> mp := Plainmap.add x i !mp) arr;
		!mp
		
	let format fmt m =
		ListUtils.format fmt (List.rev (Plainmap.fold (fun k v l -> (k,v)::l) m []))
		
end


module AvlTree = struct

	let bal_const = 1

	type 'a avltree = Empty | Node of 'a * int * 'a avltree * 'a avltree
	
	let empty = Empty
	
	let is_empty = function Empty -> true | _ -> false

	let singleton x = Node (x, 1, Empty, Empty)

	let left = function
	    Empty -> raise Not_found
	|   Node (_, _, l, _) -> l
	
	let right = function
	    Empty -> raise Not_found
	|	Node (_, _, _, r) -> r
	
	let rec min_elt = function
		Empty -> raise Not_found
	|	Node (x,_,Empty,_) -> x
	|	Node (_,_,left,_) -> min_elt left

	let rec max_elt = function
		Empty -> raise Not_found
	|	Node (x,_,_,Empty) -> x
	|	Node (_,_,_,right) -> max_elt right

	let root = function
	    Empty -> raise Not_found
	|	Node (v, _, _, _) -> v
	
	let height = function
	    Empty -> 0
	|	Node (_, h, _, _) -> h

    let height_join left right =
    	1 + Pervasives.max (height left) (height right)
    	
	let create x l r =
		Node (x, height_join l r, l, r)

	let balance x l r =
	    let hl = height l in
	    let hr = height r in
	    if hl > hr + bal_const then (
	    	match l with
	      		Empty -> invalid_arg "AvlTree.balance"
	        |	Node(lvx, _, ll, lr) ->
	          		if height ll >= height lr then
	            		create lvx ll (create x lr r)
	            	else (
			            match lr with
	        		    	Empty -> invalid_arg "AvlTree.balance"
	        		    |	Node(lrx, _, lrl, lrr)->
	                		create lrx (create lvx ll lrl) (create x lrr r)
	                )
	    )
	    else if hr > hl + bal_const then (
	    	match r with
	        	Empty -> invalid_arg "AvlTree.balance"
	        |	Node(rvx, _, rl, rr) ->
	        		if height rr >= height rl then
	            		create rvx (create x l rl) rr
	          		else (
	            		match rl with
	              			Empty -> invalid_arg "AvlTree.balance"
	            		|	Node(rlx, _, rll, rlr) ->
	                			create rlx (create x l rll) (create rvx rlr rr)
	         		)
	    )
	    else Node(x, (if hl >= hr then hl + 1 else hr + 1), l, r)
	
	let rec join v l r =
	    let rec myadd left x = function
	        Empty -> Node(x, 1, Empty, Empty)
	    |   Node(vx, _, l, r) ->
	      		if left then balance vx (myadd left x l) r
	      		        else balance vx l (myadd left x r)
	    in
	    match (l, r) with
	    	(Empty, _) -> myadd true v r
	    |	(_, Empty) -> myadd false v l
	    |	(Node(lx, lh, ll, lr), Node(rx, rh, rl, rr)) ->
		        if lh > rh + bal_const then balance lx ll (join v lr r) else
		        if rh > lh + bal_const then balance rx (join v l rl) rr else
		        create v l r

	let rec take_min = function
		Empty -> raise Not_found
	|	Node (x, _, Empty, right) -> (x, right)
	|	Node (x, _, left, right) -> let (x', left') = take_min left in (x', join x left' right)
	
	let rec take_max = function
		Empty -> raise Not_found
	|	Node (x, _, left, Empty) -> (x, left)
	|	Node (x, _, left, right) -> let (x', right') = take_max right in (x', join x left right')
	
    let reroot l r =
        if height l > height r
        then let (i, l') = take_max l in join i l' r
        else if r = Empty then Empty
        else let (i, r') = take_min r in join i l r'

	let rec take_min_iter = function
		Empty -> raise Not_found
	|	Node (x, _, Empty, right) -> (x, right)
	|	Node (x, _, Node(a, _, left, mid), right) ->
			let n = Node (x, height_join mid right, mid, right) in
			take_min_iter (Node (a, height_join left n, left, n))
	
	let take_min_iter2 = function
		Empty -> (None, Empty)
	|	t -> let (i, s) = take_min_iter t in (Some i, s)
	
	let rec take_max_iter = function
		Empty -> raise Not_found
	|	Node (x, _, left, Empty) -> (x, left)
	|	Node (x, _, left, Node(a, _, mid, right)) ->
			let n =  Node (x, height_join left mid, left, mid) in
			take_max_iter (Node (a, height_join n right, n, right))
	
	let take_max_iter2 = function
		Empty -> (None, Empty)
	|	t -> let (i, s) = take_max_iter t in (Some i, s)
	
	let iter f t =
		let rec iter_aux = function
			(None, _) -> ()
		|	(Some x, rest) -> (
				f x;
				iter_aux (take_min_iter2 rest)
			)
		in
			iter_aux (take_min_iter2 t)
	
	let fold f t a =
		let rec fold_aux a = function
			(None, _) -> a
		|	(Some x, rest) -> fold_aux (f x a) (take_min_iter2 rest)
	  	in
			fold_aux a (take_min_iter2 t)
	
	let fold_right f t a =
		let rec fold_aux a = function
			(None, _) -> a
		|	(Some x, rest) -> fold_aux (f x a) (take_max_iter2 rest)
	  	in
			fold_aux a (take_max_iter2 t)
	
	let elements t  = fold_right (fun x -> fun xs -> x::xs) t []

	let for_all f t = fold (fun x -> fun y -> (f x) && y) t true

	let exists  f t = fold (fun x -> fun y -> (f x) || y) t false

	let cardinal t = fold (fun _ a -> a + 1) t 0

	let choose = function
  		Empty -> raise Not_found
    |	Node (x,_,_,_) -> x

	(*
	type 'a decomposed_tree = 'a avltree * (('a * ('a avltree)) list)
	
	let decompose_tree t =
		(t, [])
		
	let left_expand = function
		(Empty, z) -> (Empty, z)
	|	(Node (x,_,l,r), z) -> (l, (x,r)::z)
	
	let right_discard = function
		(_, []) -> (Empty, [])
	|	(_, (x,t)::z) -> (t,z)

	let ordered_list_to_avltree l l_to_r =
		let part_by l len =
			let rec helper l acc len =
				if len = 0 then (acc, l)
				else helper (List.tl l) (List.hd l::acc) (len - 1)
			in
				helper l [] len
		in
		let rec helper l len l_to_r =
			if len = 0 then Empty else (
				let h = len / 2 in
				let (left, rest) = part_by l h in
				let (m, right) = (List.hd rest, List.tl rest) in
				let l_tree = helper left h (not l_to_r) in
				let r_tree = helper right (len - h - 1) l_to_r in
				if l_to_r
				then balance m l_tree r_tree
				else balance m r_tree l_tree
			)
		in
			helper l (List.length l) l_to_r
	*)
		
end


module IntervalSet = struct

	type 'a intervalsetfuncs = ('a -> 'a -> int) * ('a -> 'a) * ('a -> 'a) * ('a -> 'a -> int)

	type 'a intervalsetdata = ('a * 'a) AvlTree.avltree
	
	type 'a intervalset = 'a intervalsetfuncs *
	                      'a intervalsetdata

	let safe_pred (cmp,pred,_,_) limit x =
  		if cmp limit x < 0 then pred x else x

	let safe_succ (cmp,_,succ,_) limit x =
		if cmp limit x > 0 then succ x else x

	let max (cmp,_,_,_) x y =
		if cmp x y > 0 then x else y

	let min (cmp,_,_,_) x y =
		if cmp x y < 0 then x else y

    let height (_,t) =
    	AvlTree.height t

	let find_del_left (cmp,_,succ,_) p =
		let rec find = function
			AvlTree.Empty -> (p,AvlTree.Empty)
	    |	AvlTree.Node ((x,y),_,left,right) ->
				if cmp p (succ y) > 0 then
				    let (p', right') = find right in
				    (p', AvlTree.join (x, y) left right')
				else if cmp p x < 0 then find left
				else (x, left)
		in
			find
	
	let find_del_right (cmp,pred,_,_) p =
		let rec find = function
			AvlTree.Empty -> (p,AvlTree.Empty)
	    |	AvlTree.Node ((x,y),_,left,right) ->
				if cmp p (pred x) < 0 then
				    let (p', left') = find left in
				    (p', AvlTree.join (x, y) left' right)
				else if cmp p y > 0 then find right
				else (y, right)
		in
			find
                 
	let empty funcs =
  		(funcs, AvlTree.empty)
  		
  	let is_empty (_, t) =
  		AvlTree.is_empty t

	let mem z ((cmp,_,_,_), t) =
		let rec mem' = function
			AvlTree.Empty -> false 
		|	AvlTree.Node ((x, y), _, left, right) ->
				if cmp z x < 0 then mem' left
				else if cmp z y > 0 then mem' right
				else true
	in
		mem' t
		
	let min_elt (_, t) =
		fst (AvlTree.min_elt t)

	let max_elt (_, t) =
		snd (AvlTree.max_elt t)

	let add p (((cmp,pred,succ,_) as funcs),t) =
		let rec add' = function
			AvlTree.Empty -> AvlTree.Node((p, p), 1, AvlTree.Empty, AvlTree.Empty)
		|	(AvlTree.Node((x, y), h, left, right)) as t ->
      			if cmp p x >= 0
      			then if cmp p y <= 0 then t
	      			 else if cmp p (succ y) > 0
	      			 then AvlTree.join (x, y) left (add' right)
	      			 else if right = AvlTree.Empty
		             then AvlTree.Node ((x, p), h, left, right)
		             else let ((u, v), r) = AvlTree.take_min right in
	                      if pred u = p
	                      then AvlTree.join (x, v) left r
	                      else AvlTree.Node ((x, p), h, left, right)
            	else if cmp p (pred x) < 0
                then AvlTree.join (x, y) (add' left) right
                else if left = AvlTree.Empty
                then AvlTree.Node ((p, y), h, left, right)
                else let ((u, v), l) = AvlTree.take_max left in
               	     if succ v = p
               	     then AvlTree.join (u, y) l right
               	     else AvlTree.Node ((p, y), h, left, right)
		in
			(funcs, add' t)

	let insert (p, q) (((cmp,pred,succ,_) as funcs), t) =
		let rec insert' = function
	    	AvlTree.Empty -> AvlTree.Node((p, q), 1, AvlTree.Empty, AvlTree.Empty)
	    |	AvlTree.Node((x, y), _, left, right) ->
			    if cmp q (pred x) < 0
			    then AvlTree.join (x, y) (insert' left) right
			    else if cmp p (succ y) > 0
			    then AvlTree.join (x, y) left (insert' right)
			    else let (x',left') = if cmp p x >= 0 then (x,left) else find_del_left funcs p left in
			         let (y',right') = if cmp q y <= 0 then (y,right) else find_del_right funcs q right in
			         AvlTree.join (x', y') left' right'
	in
		(funcs, insert' t)

	let singleton funcs x =
		(funcs, AvlTree.singleton (x,x))

	let remove z (((cmp,pred,succ,_) as funcs), t) =
		let rec remove' = function
			AvlTree.Empty -> AvlTree.Empty
		|	AvlTree.Node ((x,y),h,left,right) ->
             	 let czx = cmp z x in
                 if czx < 0 then AvlTree.join (x, y) (remove' left) right
                 else let cyz = cmp y z in
                      if cyz < 0 then AvlTree.join (x, y) left (remove' right)
                      else if cyz = 0
                           then if czx = 0 then AvlTree.reroot left right
                                else AvlTree.Node ((x, pred y), h, left, right)
                           else if czx = 0 then AvlTree.Node ((succ x, y), h, left, right)
                                else snd (insert (succ z, y) (funcs, (AvlTree.Node ((x, pred z), h, left, right))))
		in
			(funcs, remove' t)

(*
	let rec union ((cmp,_,_,_) as funcs, input) (_, stream) =
	  	let rec union_aux input limit head stream =
	  		match head with
	  		    None -> (input, None, AvlTree.Empty)
	  		|   Some (x, y) ->
	  			  let check y (lim, superlim) =	(superlim && cmp y lim > 0) || ((not superlim) && cmp y lim >= 0) in
	  			  if check y limit then (input, Some (x, y), stream)
	  		      else match input with
	  		      			AvlTree.Empty -> let (head, stream) = AvlTree.take_min_iter2 stream in
	  		      			        ((AvlTree.Node ((x, y), 1, AvlTree.Empty, AvlTree.Empty)), head, stream)
	  		      	   |	AvlTree.Node ((a, b), _, left, right) as n ->
	  		      	   			let lim = safe_pred funcs y a in
	  		      	   			if cmp y lim < 0
	  		      	   			then let (left, head, stream) = union_aux left (lim, false) head stream in
	  		      	   				 union_aux ((AvlTree.join (a, b) left right)) limit head stream
	  		      	   			else if cmp x (safe_succ funcs x b) > 0
	  		      	   			     then let (right, head, stream) = union_aux right limit head stream in
	  		      	   			     	  union_aux ((AvlTree.join (a, b) left right)) limit head stream
	  		      	   				 else let (head, stream) = AvlTree.take_min_iter2 stream in
	  		      	   				      union_aux (snd (insert (x, y) (funcs, n))) limit head stream
	 	in
	 		if AvlTree.height stream > AvlTree.height input then union (funcs, stream) (funcs, input)
	 		else let limit = max_elt (funcs, stream) in
	             let (head, stream) = AvlTree.take_min_iter2 stream in
	             let (result, _, _) = union_aux input (limit, true) head stream in
	             (funcs, result)
*)

	let rec union ((cmp,pred,succ,_) as funcs, input) (_, stream) =
	  	let rec union_aux input limit head stream =
	  		match head with
	  		    None -> (input, None, AvlTree.Empty)
	  		|   Some (x, y) ->
	  				match input with
	  					AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  				|	AvlTree.Node ((a, b), _, left, right) ->
	  						let (left', head, stream) = if cmp x a < 0 then union_aux left (Some (pred a)) head stream
	  						                                           else (left, head, stream)
	  						in
	  							union_left_done left' (a, b) right limit head stream
		and union_left_done left (a, b) right limit head stream =
	  		match head with
	  		    None -> (AvlTree.join (a,b) left right, None, AvlTree.Empty)
	  		|   Some (x, y) ->
	  				let greater_limit z =
	  					match limit with
	  						None -> false
	  					|	Some u -> cmp z u >= 0
	  				in
	  				if (cmp y a < 0) && (cmp y (pred a) < 0)
	  				then let left' = snd (insert (x, y) (funcs, left)) in
	  				     let (head, stream) = AvlTree.take_min_iter2 stream in 
	  				     union_left_done left' (a, b) right limit head stream
	  				else if (cmp x b > 0) && (cmp x (succ b) > 0)
	  				then let (right', head, stream) = union_aux right limit head stream in
	  				     (AvlTree.join (a,b) left right', head, stream)
	  				else if cmp b y >= 0
	  				then let (head, stream) = AvlTree.take_min_iter2 stream in 
	  					 union_left_done left (min funcs a x, b) right limit head stream
	  				else if greater_limit y
	  				then (left, Some (min funcs a x, y), stream)
	  				else let (right', head, stream) = union_aux right limit (Some (min funcs a x, y)) stream in
	  				     (AvlTree.reroot left right', head, stream)
	 	in
	 		if AvlTree.height stream > AvlTree.height input then union (funcs, stream) (funcs, input)
	 		else let (head, stream) = AvlTree.take_min_iter2 stream in
	             let (result, head, stream) = union_aux input None head stream in
	             match head with
	             	None -> (funcs, result)
	             |	Some i -> let r = AvlTree.join i result stream in
	                          (funcs, r)

  	let iter f ((cmp,_,succ,_), t) =
		let g (x, y) =
			let z = ref x in
			while cmp !z y < 0 do
				f !z;
				z := succ !z;
			done;
			f !z
		in
			AvlTree.iter g t


	let fold f ((cmp,_,succ,_), t) a =
		let rec g (x, y) a =
		  if cmp x y < 0
		  then g (succ x, y) (f x a)
		  else f x a
		in
			AvlTree.fold g t a

	let fold_right f ((cmp,pred,_,_), t) a =
		let rec g (x, y) a =
		  if cmp x y < 0
		  then g (x, pred y) (f y a)
		  else f y a
		in
			AvlTree.fold_right g t a


	let elements t  =
  		fold_right (fun x -> fun xs -> x::xs) t []


	let for_all f t =
		fold (fun x -> fun y -> (f x) && y) t true


	let exists f t =
		fold (fun x -> fun y -> (f x) || y) t false

	let filter f ((cmp,pred,_,_) as funcs, t) =
  		let rec filter' = function
  			AvlTree.Empty -> AvlTree.Empty
  		|	AvlTree.Node ((x,y),_,left,right) ->
           		let z = ref y in
           		let z' = ref y in
           		let s = ref y in
           		let ivs = ref [] in
           		let good = ref false in
           		while cmp x !z < 0 do
           			if (not !good) && (f !z) then (
           				good := true;
           				s := !z
           			)
           			else if !good && (not (f !z)) then (
           				good := false;
           				ivs := (!z', !s)::!ivs
           			);
           			z' := !z;
           			z := pred !z
           		done;
           		if (f !z)
           		then ivs := (if !good then (!z, !s) else (!z, !z))::!ivs
           		else if !good then ivs := (!z', !s)::!ivs;
           		match !ivs with
           			[] -> AvlTree.reroot (filter' left) (filter' right)
           		|   i::is -> List.fold_left (fun x y -> snd (insert y (funcs, x))) (AvlTree.join i (filter' left) (filter' right)) is
		in
			(funcs, filter' t)


	let cardinal ((_,_,_,dist),t) =
	    let rec cardinal_aux a = function
	    	[] -> a
	    |	AvlTree.Empty::ts -> cardinal_aux a ts
	    |	(AvlTree.Node ((x,y),_,left,right))::ts -> cardinal_aux (a+(dist x y)+1) (left::right::ts)
	    in
		    cardinal_aux 0 [t]


	let choose t =
  		fst (AvlTree.choose (snd t))

	let split x ((cmp,pred,succ,_) as funcs, t) =
		let rec split' = function
        	AvlTree.Empty ->
            	(AvlTree.Empty, false, AvlTree.Empty)
      	|	AvlTree.Node((a, b), _, l, r) ->
	      		let cxa = cmp x a in
	      		if cxa < 0
	      		then let (ll, pres, rl) = split' l in (ll, pres, AvlTree.join (a, b) rl r)
	      		else let cbx = cmp b x in
	      		     if cbx < 0
	      		     then let (lr, pres, rr) = split' r in (AvlTree.join (a, b) l lr, pres, rr)
	      		     else ((if cxa = 0 then l else snd (insert (a, pred x) (funcs, l))),
	      		           true,
	      		           (if cbx = 0 then r else snd (insert (succ x, b) (funcs, r))))
		in
			let (l, b, r) = split' t in
			((funcs, l), b, (funcs, r))
			
	let rec inter ((cmp, pred, succ, _) as funcs, input) (_, stream) =
	  	let rec inter_aux input head stream =
	  		match head with	None -> (AvlTree.Empty, None, AvlTree.Empty)
	  		|   Some (x, y) -> match input with
	  				AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  			|	AvlTree.Node ((a, b), _, left, right) ->
	  					let (left, head, stream) = if cmp x a < 0 then inter_aux left head stream
	  					                           else (AvlTree.Empty, head, stream) in
	  					inter_left_done (a, b) right left head stream
	  	and inter_left_done (a, b) right left head stream =
	  		match head with	None -> (left, None, AvlTree.Empty)
	  		|	Some (x, y) ->
	  			let cya = cmp y a in
	  			if cya < 0 then if stream = AvlTree.Empty then (left, None, AvlTree.Empty)
	  							else let (head, stream) = AvlTree.take_min_iter stream in
	  							     inter_left_done (a, b) right left (Some head) stream
	  			else let cbx = cmp b x in
	                 if cbx < 0 then let (right, head, stream) = inter_aux right head stream in
	                                 (AvlTree.reroot left right, head, stream)
	                 else if cmp y (safe_pred funcs y b) >= 0
	                      then let (right, head, stream) = inter_aux right head stream in
	                           ((AvlTree.join (max funcs x a, min funcs y b) left right), head, stream)
	                      else let left = snd (insert (max funcs x a, y) (funcs, left)) in
	                           inter_left_done (succ y, b) right left head stream
	 	in
	 		if AvlTree.height stream > AvlTree.height input
	 		then inter (funcs, stream) (funcs, input)
	 		else if stream = AvlTree.Empty then (funcs, AvlTree.Empty)
	             else let (head, stream) = AvlTree.take_min_iter stream in
	                  let (result, _, _) = inter_aux input (Some head) stream in
	                  (funcs, result)

	(*
	let inter2 ((cmp, pred, succ, _) as funcs, s) (_, t) =
		let rec inter' acc s t = match (s, t) with
			((AvlTree.Empty,[]), _) -> acc
		|	(_, (AvlTree.Empty,[])) -> acc
		|	((_, []), _) -> inter' acc (AvlTree.left_expand s) t
		|	(_, (_, [])) -> inter' acc s (AvlTree.left_expand t)
		|	((s_hd,((xs,ys),_)::_),(t_hd,((xt,yt),_)::_)) ->
				let x_cmp = cmp xs xt in
				if x_cmp < 0 && (t_hd != AvlTree.Empty) then inter' acc s (AvlTree.left_expand t)
				else if x_cmp > 0 && (s_hd != AvlTree.Empty) then inter' acc (AvlTree.left_expand s) t
				else if x_cmp = 0 && (s_hd != AvlTree.Empty) && (t_hd != AvlTree.Empty) then inter' acc (AvlTree.left_expand t) (AvlTree.left_expand s)
				else if cmp ys xt < 0 then inter' acc (AvlTree.right_discard s) t
				else if cmp yt xs < 0 then inter' acc s (AvlTree.right_discard t)
				else let y_cmp = cmp ys yt in
				     let y' = if y_cmp >= 0 then yt else ys in
				     let x' = if x_cmp >= 0 then xs else xt in
				     let acc' = (x', y')::acc in
				     let both_discard = (y_cmp = 0) ||
				                        ((y_cmp > 0) && (cmp (succ yt) ys = 0)) ||
				                        ((y_cmp < 0) && (cmp (succ ys) yt = 0)) in
				     if both_discard then inter' acc' (AvlTree.right_discard s) (AvlTree.right_discard t)
				     else if y_cmp > 0 then inter' acc' s (AvlTree.right_discard t)
				     else inter' acc' (AvlTree.right_discard s) t
		in
		(funcs, AvlTree.ordered_list_to_avltree (inter' [] (AvlTree.decompose_tree s) (AvlTree.decompose_tree t)) false)
	*)

	let diff ((cmp,pred,succ,_) as funcs, input) (_, stream) =
	  	let rec diff_aux input head stream =
	  		match head with	None -> (input, None, AvlTree.Empty)
	  		|   Some (x, y) -> match input with
	  				AvlTree.Empty -> (AvlTree.Empty, head, stream)
	  			|	AvlTree.Node ((a, b), _, left, right) ->
	  					let (left, head, stream) = if cmp x a < 0 then diff_aux left head stream
	  					                           else (left, head, stream) in
	  					diff_left_done (a, b) right left head stream
	  	and diff_left_done (a, b) right left head stream =
	  		match head with	None -> (AvlTree.join (a, b) left right, None, AvlTree.Empty)
	  		|	Some (x, y) ->
	  			let cya = cmp y a in
	  			if cya < 0 then let (head, stream) = AvlTree.take_min_iter2 stream in
	        				    diff_left_done (a, b) right left head stream
	  			else let cbx = cmp b x in
	                 if cbx < 0 then let (right, head, stream) = diff_aux right head stream in
	                                 (AvlTree.join (a, b) left right, head, stream)
	                 else if cmp a x < 0
	                      then diff_left_done (x, b) right (snd (insert (a, pred x) (funcs, left))) head stream
	                      else if cmp y b < 0
	                           then let (head, stream) = AvlTree.take_min_iter2 stream in
	                                diff_left_done (succ y, b) right left head stream
	                           else let (right, head, stream) = diff_aux right head stream in
	                           		(AvlTree.reroot left right, head, stream)
	 	in
	 		if stream = AvlTree.Empty then (funcs, input)
	        else let (head, stream) = AvlTree.take_min_iter stream in
	             let (result, _, _) = diff_aux input (Some head) stream in
	             (funcs, result)
	
	let compare ((cmp,_,_,_), t1) (_, t2) =
		let rec compare' t1 t2 =
		  	if (t1 != AvlTree.Empty) && (t2 != AvlTree.Empty)
		  	then let ((ix1, iy1), r1) = AvlTree.take_min_iter t1 in
		  		 let ((ix2, iy2), r2) = AvlTree.take_min_iter t2 in
		  		 let d = cmp ix1 ix2 in
		  		 let c = if d != 0 then -d else cmp iy1 iy2 in
		  		 if c != 0 then c else compare' r1 r2
		  	else if t1 != AvlTree.Empty then 1
		  	else if t2 = AvlTree.Empty then 0 else -1
		in
			compare' t1 t2


	let equal t1 t2 =
		compare t1 t2 = 0

(*
	let subset ((cmp,_,succ,_), t1) (_, t2) =
		let rec subset' t1 t2 =
		  	if t1 = AvlTree.Empty || t2 = AvlTree.Empty then t1 = AvlTree.Empty
		  	else let ((x1, y1), r1) = AvlTree.take_min_iter t1 in
		  	     let ((x2, y2), r2) = AvlTree.take_min_iter t2 in
		  	     if cmp x1 x2 < 0 then false
		  	     else let upper = cmp y1 y2 in
		  	          if upper > 0 then subset' (AvlTree.Node ((succ y2, y1), 1 + AvlTree.height r1, AvlTree.Empty, r1)) r2
		  	          else if upper = 0 then subset' r1 r2
		  	          else subset' r1 (AvlTree.Node ((succ y1, y2), 1 + AvlTree.height r2, AvlTree.Empty, r2))
		in
			subset' t1 t2
*)

	let subset ((cmp,_,_,_), t1) (_, t2) =
		let rec subset' ((x1, y1), r1) ((x2, y2), r2) =
	  	     if cmp x1 x2 < 0 then false
	  	     else if cmp x1 y2 > 0 then if r2 = AvlTree.Empty then false
	  	                                else subset' ((x1, y1), r1) (AvlTree.take_min_iter r2)
	  	     else let upper = cmp y1 y2 in
	  	     	  if upper < 0 then if r1 = AvlTree.Empty then true
	  	     	                    else subset' (AvlTree.take_min_iter r1) ((x2, y2), r2)
	  	     	  else if upper = 0 then if r1 = AvlTree.Empty || r2 = AvlTree.Empty then r1 = AvlTree.Empty
	  	     	                         else subset' (AvlTree.take_min_iter r1) (AvlTree.take_min_iter r2)
	  	     	  else false
		in
		  	if t1 = AvlTree.Empty || t2 = AvlTree.Empty then t1 = AvlTree.Empty
		  	else subset' (AvlTree.take_min_iter t1) (AvlTree.take_min_iter t2)


	let partition f ((cmp, pred, _, _) as funcs, t) =
		let rec partition' = function
			AvlTree.Empty -> (AvlTree.Empty, AvlTree.Empty)
		|	AvlTree.Node ((x,y),_,left,right) ->
           		let z = ref y in
           		let z' = ref y in
           		let sg = ref y in
           		let sb = ref y in
           		let ivsg = ref [] in
           		let ivsb = ref [] in
           		let good = ref false in
           		let starters = ref true in
           		while cmp x !z < 0 do
           			if (not !good) && (f !z) then (
           				good := true;
           				sg := !z;
           				if not !starters then ivsb := (!z', !sb)::!ivsb
           			)
           			else if !good && (not (f !z)) then (
           				good := false;
           				sb := !z;
           				ivsg := (!z', !sg)::!ivsg
           			);
           			z' := !z;
           			z := pred !z;
           			starters := false
           		done;
           		if (f !z)
           		then (
           			ivsg := (if !good then (!z, !sg) else (!z, !z))::!ivsg;
           			if not (!good || !starters) then ivsb := (!z', !sb)::!ivsb
           		)
           		else (
           			ivsb := (if not !good then (!z, !sb) else (!z, !z))::!ivsb;
           			if !good then ivsg := (!z', !sg)::!ivsg
           		);
           		let (leftg, leftb) = partition' left in
           		let (rightg, rightb) = partition' right in
           		((
                    match !ivsg with
                        [] -> AvlTree.reroot leftg rightg
                    |   i::is -> List.fold_left (fun x y -> snd (insert y (funcs,x))) (AvlTree.join i leftg rightg) is
                ),(
                    match !ivsb with
                        [] -> AvlTree.reroot leftb rightb
                    |   i::is -> List.fold_left (fun x y -> snd (insert y (funcs,x))) (AvlTree.join i leftb rightb) is
                ))
		in
			let (l, r) = partition' t in
			((funcs, l), (funcs, r))

	let count f t =
		fold (fun x c -> if f x then c + 1 else c) t 0

end

module IntervalSetFuncs = struct
	
	let int_intervalset =
		(compare, (fun x -> x-1), (fun x -> x+1), (fun x y -> y-x))

end	