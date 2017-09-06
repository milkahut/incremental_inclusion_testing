open Nba;;

module MH = Map.Make(struct
  type t = state
  let compare = compare
end);;

type position = SpoilerNode of state * (bool MH.t)  
              | DuplicatorNode of state * (bool MH.t) * symbol
     	      | SpoilerTrap
              | DuplicatorTrap

let compare = compare


let auta = ref empty_nba
let autb = ref empty_nba
let pebbles = ref 0

let setup a b k = auta := a; autb := b; pebbles := k

let owner = function SpoilerTrap           -> 0 
                   | DuplicatorTrap        -> 1
       	           | SpoilerNode(_,_)      -> 1
      	           | DuplicatorNode(_,_,_) -> 0

let priority = function SpoilerTrap           -> 0
                      | DuplicatorTrap        -> 1
      		      | SpoilerNode(q,s)      -> if not (MH.exists (fun _ -> fun b -> b) s) then 2 
                                                 else if is_final !auta q then 1 else 0
      		      | DuplicatorNode(q,s,_) -> 0 

let short_show s = "{" ^ String.concat "," (List.map (fun (q,_) -> show_state q) 
                                                     (List.filter (fun (_,x) -> x) (MH.bindings s))) 
                   ^ "}+{" ^ String.concat "," (List.map (fun (q,_) -> show_state q) 
                                                         (List.filter (fun (_,x) -> not x) (MH.bindings s))) ^ "}"

let long_show s = "({" ^ String.concat "," (List.map (fun (q,_) -> show_state q) 
                                                    (List.filter snd (MH.bindings s))) 
                   ^ "},{" ^ String.concat "," (List.map (fun (q,_) -> show_state q) 
                                                         (MH.bindings s)) ^ "})"

let short_desc = function SpoilerTrap -> "1-trap"
                        | DuplicatorTrap -> "0-trap"
      	                | SpoilerNode(q,s) -> show_state q ^ "," ^ short_show s
      		        | DuplicatorNode(q,s,a) -> show_state q ^ "," ^ short_show s ^ "," ^ show_symbol a

let long_desc = function SpoilerTrap -> "SpoilerTrap"
                       | DuplicatorTrap -> "DuplicatorTrap"
                       | SpoilerNode(q,s) -> "Sp(" ^ show_state q ^ "," ^ long_show s ^ ")"
       	               | DuplicatorNode(q,s,a) -> "Du(" ^ show_state q ^ "," ^ long_show s ^ "," ^ show_symbol a ^ ")"

(*
let initial _ = let qa = initial !auta in
                let qb = initial !autb in
                SpoilerNode(qa, MH.singleton qb (not (is_final !autb qb)))
*)

let successors = function
           SpoilerTrap      -> [ SpoilerTrap ] 
         | DuplicatorTrap   -> [ DuplicatorTrap ] 
         | SpoilerNode(q,s) -> let spoiler_choices = free_choices !auta q 1 in
                               if spoiler_choices = [] then [ SpoilerTrap ]
                               else List.map (fun (q',w,_) -> DuplicatorNode(q',s,List.hd w)) spoiler_choices 
         | DuplicatorNode(qa,s,a) -> 
             let (x,y) = MH.partition (fun _ -> fun x -> x) s in
             let bp = not (MH.exists (fun _ -> fun _ -> true) x) in 
             let nextmh = 
               let auxmh = MH.fold 
                            (fun q -> fun _ -> fun t -> List.fold_left 
                                                          (fun t -> fun q' -> MH.add q' (bp && (not (is_final !autb q'))) t)
                                                          t
                                                          (List.map fst (guided_choices !autb q [a])))
                            y
                            MH.empty 
               in
               MH.fold 
                  (fun q -> fun _ -> fun t -> List.fold_left 
                                                (fun t -> fun (q',x') -> MH.add q' x' t)
                                                t
                                                (List.map (fun (q',_) -> (q',not (is_final !autb q'))) 
                                                          (guided_choices !autb q [a])))
                  x
                  auxmh
             in
             let subs = Array.make (!pebbles + 1) [] in
             subs.(0) <- [ MH.empty ];
             List.iter (fun (e,x) -> for j=(!pebbles-1) downto 0 do
                                       List.iter (fun t -> subs.(j+1) <- (MH.add e x t) :: subs.(j+1)) subs.(j) 
                                     done)
                       (MH.bindings nextmh);
             let rec collect i todo acc = if i=0 then acc 
                                          else match todo with
                                                     []      -> collect (i-1) (subs.(i-1)) acc 
				                   | (t::ts) -> collect i     ts           (t::acc)
             in
             let succs = collect !pebbles subs.(!pebbles) [] in
(*
             let rec contains_duplicates = function []      -> false
	                                          | (x::xs) -> List.mem x xs || contains_duplicates xs
             in
             if contains_duplicates succs then
               begin
                 print_string ("\n\n PANIC!!! Successor list of node " ^ long_desc (DuplicatorNode(qa,s,a)) ^ " contains duplicates:\n");
                 List.iter (fun t -> print_string ((long_show t) ^ "\n")) succs;
                 print_newline ();
                 print_string ("These were created from the following elements: " ^ String.concat "," (List.map (fun (e,x) -> "(" ^ show_state e ^ "," ^ string_of_bool x ^ ")") (MH.bindings nextmh)));
                 print_newline ();
                 exit 1
               end; *)
             List.map (fun s -> SpoilerNode(qa,s)) succs




let make_pos qa qb = SpoilerNode(qa, MH.singleton qb (not (is_final !autb qb)))

module Local = struct
  type pos = position
  let compare = compare
  let setup = setup
  let owner = owner
  let priority = priority
  let short_desc = short_desc
  let long_desc = long_desc
  let successors = successors
  let initials _ = [ make_pos (initial !auta) (initial !autb) ] 
  let make_pos = make_pos
end;;

module Global = struct
  type pos = position
  let compare = compare
  let setup = setup
  let owner = owner
  let priority = priority
  let short_desc = short_desc
  let long_desc = long_desc
  let successors = successors
  let initials _ = let acc = ref [] in
                   for i = (size !auta)-1 downto 0 do
                     for j = (size !autb)-1 downto 0 do
                       acc := (make_pos (make_state i) (make_state j)) :: !acc
                     done
                   done;
                   !acc
  let make_pos = make_pos
end;;
