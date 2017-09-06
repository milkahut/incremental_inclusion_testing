open Basics ;;
open Tcsset;;
open Tcsarray;;
open Paritygame ;;
open Graph ;;
open Univsolve;;
open Solvers;;
open Transformations;;


let solve' (game: paritygame) =
	let array_max a less = ArrayUtils.max_elt (fun x y -> if less x y then -1 else 1) a in

	let n = Array.length game in

    let msg_tagged v = message_autotagged v (fun _ -> "STRATIMPROV") in
    let msg_plain = message in


    let prio i = let (pr, _, _, _) = game.(i) in pr in
    let pl i = let (_, pl, _, _) = game.(i) in pl in
    let tra i = let (_, _, delta, _) = game.(i) in delta in
    let isEven i = (prio i) mod 2 = 0 in
    let isOdd i = (prio i) mod 2 = 1 in
    let isP0 i = (pl i) = 0 in
    let isP1 i = (pl i) = 1 in

    let lessRel i j =
        let pi = prio i in
        let pj = prio j in
            pi < pj || (pi = pj && i < j)
    in

    let lessRew i j =
        let pi = prio i in
        let pj = prio j in
        let fi = if pi mod 2 = 0 then 1 else -1 in
        let fj = if pj mod 2 = 0 then 1 else -1 in
            fi * pi < fj * pj || (pi = pj && fi * i < fj * j)
    in

    let module AscRewOrder = struct
      type t = int
      let compare x y = if lessRew x y then -1 else if lessRew y x then 1 else 0
    end in

    let module AscRewSet = Set.Make(AscRewOrder) in

    let module DescRelOrder = struct
      type t = int
      let compare x y = if lessRel x y then 1 else if lessRel y x then -1 else 0
    end in

    let module DescRelSet = Set.Make(DescRelOrder) in

    let module Graph =
       struct
         let make n =
         	(ref AscRewSet.empty, ref DescRelSet.empty, Hashtbl.create n)

         let size (_, _, t) = Hashtbl.length t

         let hasNode (a, d, t) n =
         	Hashtbl.mem t n

         let hasEdge (a, d, t) n m =
         	(hasNode (a, d, t) n) && IntSet.mem m (snd (Hashtbl.find t n))

         let addNode (a, d, t) n =
         	if hasNode (a, d, t) n
         	then ()
         	else (a := AscRewSet.add n !a;
         	      d := DescRelSet.add n !d;
         	      Hashtbl.add t n (IntSet.empty, IntSet.empty))

         let addEdge (a, d, t) n m =
         	addNode (a, d, t) n;
         	addNode (a, d, t) m;
         	let (prevn, nextn) = Hashtbl.find t n in
         	Hashtbl.replace t n (prevn, IntSet.add m nextn);
         	let (prevm, nextm) = Hashtbl.find t m in
         	Hashtbl.replace t m (IntSet.add n prevm, nextm)

         let delEdge (a, d, t) n m =
         	if hasEdge (a, d, t) n m
         	then let (prevn, nextn) = Hashtbl.find t n in
                 Hashtbl.replace t n (prevn, IntSet.remove m nextn);
                 let (prevm, nextm) = Hashtbl.find t m in
                 Hashtbl.replace t m (IntSet.remove n prevm, nextm)
         	else ()

         let delNode (a, d, t) n =
         	if hasNode (a, d, t) n
         	then let (prev, next) = Hashtbl.find t n in
         		 (a := AscRewSet.remove n !a;
         	      d := DescRelSet.remove n !d;
         	      IntSet.iter (fun i -> delEdge (a, d, t) i n) prev;
         	      IntSet.iter (fun i -> delEdge (a, d, t) n i) next;
         	      Hashtbl.remove t n)
         	else ()

         let prev (a, d, t) n =
         	fst (Hashtbl.find t n)

         let next (a, d, t) n =
         	snd (Hashtbl.find t n)

         let iterAsc (a, d, t) f = AscRewSet.iter f (!a)

         let iterDesc (a, d, t) f = DescRelSet.iter f (!d)

         let iter (a, d, t) f = Hashtbl.iter f t

         let subGraphByEdgePred gr pred =
         	let gr' = make 10 in
         	let add n e = if pred n e then addEdge gr' n e else () in
         		iter gr (fun n (_, e) -> IntSet.iter (add n) e);
         		gr'

         let subGraphByNodeClosure (a, d, t) nex init =
         	let gr' = make 10 in
         	let rec iterate x =
         		if hasNode gr' x then ()
         		else (add x; IntSet.iter iterate (nex x))
         	and add n =
         		addNode gr' n;
         		let (pr, ne) = Hashtbl.find t n in
         		(IntSet.iter (fun x -> if (hasNode gr' x)
         		                       then addEdge gr' x n else ()) pr;
         		 IntSet.iter (fun x -> if (hasNode gr' x)
         		                       then addEdge gr' n x else ()) ne) in
           iterate (init);
           gr'

           let formatgraph (a, d, t) =
           	Hashtbl.fold (fun n (_, e) s -> s ^ (string_of_int n) ^ (IntSetUtils.format e) ^ "\n") t ""

    end in

    let formatvaluentry (a, b, c) =
         let t = DescRelSet.fold (fun el s -> s ^ (string_of_int el) ^", ") b "" in
         (string_of_int a) ^ " [" ^ t ^ "] " ^ (string_of_int c)
    in
    (*
    let formatvalu valu =
    	let s = ref "" in
		for i = 0 to n - 1 do
    		s := !s ^ (string_of_int i) ^ " : " ^ formatvaluentry valu.(i) ^ "\n"
    	done;
    	!s
    in
*)
    let lessValu valu x y =
      let symdiff (i: DescRelSet.t) (j: DescRelSet.t) =
          DescRelSet.union (DescRelSet.diff i j) (DescRelSet.diff j i)
      in
      let (u, p, e) = valu.(x) in
      let (v, q, f) = valu.(y) in
        (lessRew u v) ||
        ((u = v) &&
        (let r = symdiff p q in
           if DescRelSet.cardinal r = 0 then
             ((isOdd v) && e < f) || ((isEven v) && e > f)
           else
             let w = DescRelSet.min_elt r in
             ((isEven w) && DescRelSet.mem w q) || ((isOdd w) && DescRelSet.mem w p)))
    in


    let subvaluation valu gr w =
      let rec buildReachSet w t s =
      	IntSet.fold (fun v s' -> if IntSet.mem v s' then s'
      	                         else buildReachSet v t (IntSet.add v s')) (t w) s
      in
      let innerLoop u =
        let adder' m u v =
          if IntSet.mem v m then ()
          else let (a, b, c) = valu.(v) in
          	valu.(v) <- (a, DescRelSet.add u b, c);
        in
        let deleter m x =
          IntSet.iter (fun y -> if IntSet.mem y m
                                then () else (
                                	Graph.delEdge gr x y;
                                )) (Graph.next gr x)
        in
            if not (lessRel w u) then ()
            else (
				 if isEven u
            then let m = buildReachSet w (fun x -> IntSet.filter ((!=) u) (Graph.prev gr x))
                                         (IntSet.singleton w) in
            		(
            		 Graph.iter gr (fun v _ -> adder' m u v);
            		 deleter m u;
            		 IntSet.iter (deleter m) m)
            else let m = buildReachSet u (fun x -> IntSet.filter ((!=) w) (Graph.prev gr x))
                                         (IntSet.singleton u) in
            		(
            		 IntSet.iter (fun v ->
            		               let (a, b, c) = valu.(v) in
            		               	valu.(v) <- (a, DescRelSet.add u b, c);

            		               	) m;
            		 IntSet.iter (fun x -> if x = u then () else deleter m x) m)
        )
      in
    	Graph.iter gr (fun x _ ->  valu.(x) <- (w, DescRelSet.empty, 0));
    	Graph.iterDesc gr innerLoop;
    	let distIter n i = let (p0, p1, _) = valu.(n) in valu.(n) <- (p0, p1, i) in
		if isEven w
		then max_dist_loop_closure w (fun x -> IntSet.elements (Graph.prev gr x))
		                             (fun x -> IntSet.elements (Graph.next gr x)) distIter
		else min_dist_closure w (fun x -> IntSet.elements (Graph.prev gr x)) (fun _ -> distIter) ()
    in


	let valuation valu gr =
		let work w =
			let gr' = Graph.subGraphByNodeClosure gr (Graph.prev gr) w in
            let delEdge x y =
            	if Graph.hasNode gr' y
            	then ()
            	else (Graph.delEdge gr x y)
            in
				subvaluation valu gr' w;
				Graph.iter gr' (fun x _ -> IntSet.iter (delEdge x) (Graph.next gr x));
		in
		let iter n =
            let prev' k =
                IntSet.elements (
                  IntSet.filter (fun k' -> (n = k' || lessRel k' n)) (Graph.prev gr k)
                              )
            in
			let (w, _, _) = valu.(n) in
				if (w = -1) && (depth_exists n prev' (fun x -> Graph.hasEdge gr n x))
				then work n else ()
		in

(*			msg_tagged 3 (fun _ -> "Entering valuation on:\n");
			msg_tagged 3 (fun _ -> Graph.formatgraph gr); *)
            Graph.iterAsc gr iter;
(*            msg_tagged 3 (fun _ -> "Made valuation:\n");
            msg_tagged 3 (fun _ -> (formatvalu valu)^"\n") *)
	in


    (* ----- Solve ----- *)

    msg_tagged 2 (fun _ -> "Starting strategy improvement algorithm\n");
    if !verbosity > 2 then (
	    msg_tagged 3 (fun _ -> "Considering game: \n");
	    print_game game
	);

    let valu = Array.make n (-1, DescRelSet.empty, 0) in
    let strategy = Array.make n (-1) in
    let v0 = ref IntSet.empty in
    let ggraph = Graph.make n in


    (* Initial v0 + strategy *)
    for i = 0 to n - 1 do
    	if isP1 i
    	then ()
    	else (strategy.(i) <- array_max (tra i) lessRew;
    	      v0 := IntSet.add i (!v0))
    done;

    (* Game graph *)
    for i = 0 to n - 1 do
    	Graph.addNode ggraph i;
    	let arr = tra i in
    		for j = 0 to (Array.length arr) - 1 do
    			Graph.addEdge ggraph i arr.(j)
    		done;
    done;

    (* Strategy improvement *)
    let changed = ref true in
    let counter = ref 0 in

    let selPred u v = (isP1 u) || (strategy.(u) = v) in
    	while (!changed) do
   		    incr counter;
   		    msg_tagged 2 (fun _ -> "Iteration: " ^ string_of_int !counter ^ "\r");
    		changed := false;
    		Array.fill valu 0 n (-1, DescRelSet.empty, 0);
    		valuation valu (Graph.subGraphByEdgePred ggraph selPred);
    		if !verbosity > 2 then (
    			let g = subgame_by_edge_pred game selPred in
    			for i = 0 to n - 1 do
    				pg_set_desc g i (Some ((string_of_int i) ^ " : " ^ formatvaluentry valu.(i)))
    			done;
    			msg_tagged 3 (fun _ -> "Made valuation:\n");
    			print_game g
    		);
    		let stratUpd x =
    			let w = array_max (tra x) (lessValu valu) in
    				if lessValu valu strategy.(x) w
    				then (strategy.(x) <- w;
    				      changed := true)
    			    else ()
    		in
    			IntSet.iter stratUpd (!v0)
    	done;

    msg_plain 2 (fun _ -> "\n");

    (* Finished *)
    let solution = Array.map (fun (w, _, _) -> (prio w) mod 2) valu in
    let strategy = Array.make n (-1) in
    	for i = 0 to n - 1 do
    		strategy.(i) <-	if isP0 i
    					    then array_max (tra i) (lessValu valu)
    					    else array_max (tra i) (fun x y -> (lessValu valu) y x)
    	done;
    (solution, strategy);;

(*
let solve'' game =
	let game' = alternating_transformation game true in
	let (sol, strat) = solve' game' in
	let (sol', strat') = alternating_revertive_restriction game game' sol strat in
	for i = 0 to Array.length game - 1 do
		let (_, pl, _, _) = game.(i) in
		if sol'.(i) != pl then strat'.(i) <- -1
	done;
	(sol', strat');;
*)

let solve game =
	let opt = (universal_solve_init_options_verbose !universal_solve_global_options) in
	universal_solve opt solve' game;;


register_solver solve "stratimprove" "si" "use the strategy improvement algorithm due to Jurdzinski / Voege";;


