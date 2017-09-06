open Tcsset;;
open Tcslist;;
open Tcsautomata;;

type 'a ctree = Node of int * 'a list * 'a ctree list
type 'a state = 'a ctree * int

type 'a internal_tree = IntNode of int * ('a Plainset.plainset) * 'a internal_tree list

let piterman_nba_to_dpa (((start, delta, final), (size', cmp_state', cmp_rule), (fmt_state, fmt_rule)): ('s, 'r) nba) =
	let size = match size' with
		Some s -> s
	|	None -> Pervasives.max_int / 2
	in
	let cmp_state = match cmp_state' with
		Some cmp -> cmp
	|	None -> failwith "piterman_nba_to_dpa: Compare required."
	in

    let rec tree_to_internal = function
    	Node (n, s, c) -> IntNode (n, Plainset.set_of_list cmp_state s, List.map tree_to_internal c)
    in
    
    let rec internal_to_tree = function
    	IntNode (n, s, c) -> Node (n, Plainset.elements s, List.map internal_to_tree c)
	in

    let fmt_state' (tree, p) =
	    let rec childrenToStr =
	        function
	            [] -> ""
	            | hd :: tl -> hd ^ ", " ^ (childrenToStr tl) in
	    let rec treeToStr2 =
	        function
	            Node (n, states, children) ->
	                let childrenDone = List.map treeToStr2 children in
	                "(" ^ (string_of_int n) ^ " " ^
	                (ListUtils.format fmt_state states) ^
	                " {" ^ (childrenToStr childrenDone) ^"})" in
	    string_of_int p ^ ": " ^ treeToStr2 tree
	in
	
	let start' = (Node (1, [start], []), 1) in

	let index = match size' with
		Some s -> Some (2 * size - 1)
	|	None -> None
	in

	let omega q = 2 * size - snd q in
	
	let rec cmp_state'' (t1, p1) (t2, p2) =
		let rec cmp_trees (Node (i1, l1, c1)) (Node (i2, l2, c2)) =
			let c = compare i1 i2 in
			if c != 0 then c
			else let c' = ListUtils.compare_lists cmp_state l1 l2 in
			     if c' != 0 then c'
			     else ListUtils.compare_lists cmp_trees c1 c2
		in
		let c = compare p1 p2 in
		if c = 0 then cmp_trees t1 t2 else c
	in
	
	let delta' (tree', _) letter =
        (* returns the maximal nodename *)
        let rec maxNode =
            let rec maximum =
                function
                    [] -> failwith "piterman.maxNode: empty list"
                    | hd :: [] -> hd
                    | hd :: tl ->
                        let maxtl = maximum tl in
                        if hd > maxtl then hd else maxtl in
            function
                IntNode (n, _, []) -> n
                | IntNode (n, _, children) ->    maximum (n :: (List.map maxNode children)) in
        (* returns all nodenames *)
        let rec getNodenames =
            function
                IntNode (n, _, children) -> 
					List.fold_left (fun s el -> IntSet.union s (getNodenames el)) (IntSet.singleton n) children
		in
        (* for calculation of e and f *)
        let rec minimum default =
            function
                [] -> default
                | hd :: tl ->
                    let mintl = minimum default tl in
                    if hd < mintl then hd else mintl in
        (* -- I -- subset-construction *)
        let rec subsetDelta =
            let compose = function q -> delta q letter in
            let iter el acc = List.fold_left (fun s' el' -> Plainset.add el' s') acc (compose el) in
            function
                IntNode (n, states, children) ->
                   IntNode (n, Plainset.fold iter states (Plainset.empty cmp_state),
                            List.map subsetDelta children) in
        (* -- II -- split final states *)
        let rec splitFinalStates minFreeName tree =
          let mfn = ref minFreeName in
            let rec splitFStates =
                function
                    IntNode (n, states, children) ->
                        let fStates = Plainset.filter final states in
                        if Plainset.is_empty fStates then
                            let childrenDone = List.map splitFStates children in
                            IntNode (n, states, childrenDone)
                        else
                            let childrenDone = List.map splitFStates children in
                            let newChild = IntNode (!mfn, fStates, []) in
                            mfn := !mfn + 1;
                            IntNode (n, states, newChild :: childrenDone) in
            splitFStates tree in
	  (* -- III. -- "horizontal" merge *)
      let rec hMerge =
         (* merge siblings:
                - union = union of labels of all older siblings
                - newLabel = label without union
            *)
         let rec mergeSiblings siblings =
                (* union of the states of all older nodes *)
                let rec unionOlderSiblings n' =
                    function
                        [] -> Plainset.empty cmp_state
                        | IntNode (n, states, _) :: tl ->
                            if n < n' then Plainset.union states (unionOlderSiblings n' tl)
                            else unionOlderSiblings n' tl in
                (* removes certain states from the labels of all descendents *)
                let rec removeNodes toRemove =
                    function
                        IntNode (n, states, []) ->
                            let newStates = Plainset.diff states toRemove in
                            IntNode (n, newStates, [])
                        | IntNode (n, states, children) ->
                            let newStates = Plainset.diff states toRemove in
                            let childrenDone = List.map (removeNodes toRemove) children in
                            IntNode (n, newStates, childrenDone) in
                function
                    [] -> []
                    | IntNode (n, states, children) :: tl ->
                        let labelsOfOlderNodes = unionOlderSiblings n siblings in
                        let newNode = removeNodes labelsOfOlderNodes (IntNode (n, states, children)) in
                        newNode :: mergeSiblings siblings tl in
            function
                IntNode (n, states, []) -> IntNode (n, states, [])
                | IntNode (n, states, children) ->
                    IntNode (n, states, List.map hMerge (mergeSiblings children children)) in
      (* -- IV. -- vertical merge *)
      let rec vMerge =
         (* union of all states of the nodes *)
         let rec unionOfStates =
                function
                    [] -> Plainset.empty cmp_state
                    | IntNode (n, states, _) :: tl -> Plainset.union states (unionOfStates tl) in
            function
                IntNode (n, states, []) -> (size+1, IntNode (n, states, []))
                | IntNode (n, states, children) ->
                    if Plainset.equal states (unionOfStates children) then
                        (n, IntNode (n, states, []))
                    else
                        let (greenNodes, childrenDone) = List.split (List.map vMerge children) in
                        let m = minimum (size+1) greenNodes in
                        (m, IntNode (n, states, childrenDone)) in
      (* -- V. -- remove empty nodes *)
        let rec removeEmptyNodes =
            function
                [] -> (size+1, [])
                | IntNode (n, states, children) :: tl ->
                	if Plainset.is_empty states then (
                        let (n', tlDone) = removeEmptyNodes tl in
                        let m = min n n' in
                        (m, tlDone)
                    ) else (
                        let (n'', childrenDone) = removeEmptyNodes children in
                        let (n', tlDone) = removeEmptyNodes tl in
                        let m = min n' n'' in
                        (m, IntNode (n, states, childrenDone) :: tlDone)
					)
		in
        (* -- VI. -- reorganize nodenames in the tree *)
        let rec reorganize removedNodes =
            (* nodes in removedNodes which are less than n *)
            let empty n = List.filter (fun x -> x < n) removedNodes in
            function
                IntNode (n, states, children) ->
                    let childrenDone = List.map (reorganize removedNodes) children in
                    let m = n - (List.length (empty n)) in
                    IntNode (m, states, childrenDone) in
       (* returns a "normalized" tree, i.e. sorted states & children *)
        let rec normalize =
            function
                IntNode (n, states, children) ->
                    let children' = List.sort compare (List.map normalize children) in
                    IntNode (n, states, children') in
        let tree = tree_to_internal tree' in     
        let tree1 = subsetDelta tree in
        let tree2 = splitFinalStates ((maxNode tree1)+1) tree1 in
        let (f, tree3) = vMerge (hMerge tree2) in
        let (e, tree4List) = removeEmptyNodes [tree3] in
        let tree4 = List.hd tree4List in
        let nodeNamesOfTree2 = getNodenames tree2 in
        let nodeNamesOfTree4 = getNodenames tree4 in
        let removedNodes = IntSet.diff nodeNamesOfTree2 nodeNamesOfTree4 in
        let tree5 = reorganize (IntSet.elements removedNodes) tree4 in
        let p = if e <= f then 2 * e - 3 else 2 * f - 2 in
        let tree6 = normalize tree5 in
        (internal_to_tree tree6, p)
	in

	((start', delta', omega),
	 (index, Some cmp_state'', cmp_rule),
	 (fmt_state', fmt_rule))