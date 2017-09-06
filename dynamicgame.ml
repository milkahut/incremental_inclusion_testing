open Nba;;

type position = SpoilerNode of state * state * int * int
              | DuplicatorNode of state * state * int * (symbol list)
	      | InitialNode of state * state
     	      | SpoilerTrap
              | DuplicatorTrap

let compare = compare

let auta = ref empty_nba
let autb = ref empty_nba
let k = ref 0 
let lengths = ref [] 

let setup a b j = 
  auta := a; 
  autb := b; 
  k := j; 
  let rec enumerate acc i = if i=0 then acc else enumerate (i::acc) (i-1) in
  lengths := enumerate [] !k


let owner = function SpoilerTrap             -> 0 
                   | DuplicatorTrap          -> 1
       	           | SpoilerNode(_,_,_,_)    -> 1
      	           | DuplicatorNode(_,_,_,_) -> 0
		   | InitialNode(_,_)        -> 0

let priority = function SpoilerTrap             -> 0
                      | DuplicatorTrap          -> 1
      		      | SpoilerNode(_,_,p,_)    -> p
      		      | DuplicatorNode(_,_,p,_) -> p
		      | InitialNode(_,_)        -> 0


let short_desc = function SpoilerTrap -> "1-trap"
                        | DuplicatorTrap -> "0-trap"
      	                | SpoilerNode(qa,qb,_,i) -> show_state qa ^ "," ^ show_state qb ^ "," ^ string_of_int i
      		        | DuplicatorNode(qa,qb,_,w) -> show_state qa ^ "," ^ show_state qb ^ "," ^ String.concat "" (List.map show_symbol w)
			| InitialNode(qa,qb) -> show_state qa ^ "," ^ show_state qb

let long_desc = function SpoilerTrap -> "SpoilerTrap"
                       | DuplicatorTrap -> "DuplicatorTrap"
      	               | SpoilerNode(qa,qb,pr,i) -> "Sp(" ^ show_state qa ^ "," ^ show_state qb ^ "," ^ string_of_int pr ^ "," ^ 
                                                    string_of_int i ^ ")" 
      		       | DuplicatorNode(qa,qb,pr,w) -> "Du(" ^ show_state qa ^ "," ^ show_state qb ^ "," ^ string_of_int pr
                                                       ^ "," ^ String.concat "" (List.map show_symbol w) ^ ")"
		       | InitialNode(qa,qb) -> "Du(" ^ show_state qa ^ "," ^ show_state qb ^ ")"

let successors = function
           SpoilerTrap       -> [ SpoilerTrap ] 
         | DuplicatorTrap    -> [ DuplicatorTrap ] 
         | SpoilerNode(qa,qb,_,i) -> let spoiler_choices = free_choices !auta qa i in
                                     if spoiler_choices = [] then [ SpoilerTrap ]
                                     else List.map (fun (q,w,f) -> DuplicatorNode(q,qb,(if f then 1 else 0),w)) spoiler_choices 
         | DuplicatorNode(qa,qb,_,w) -> let duplicator_choices = guided_choices !autb qb w in
                                        if duplicator_choices = [] then [ DuplicatorTrap ]
                                        else
                                            List.concat
                                              (List.map (fun i -> List.map (fun (q,f) -> SpoilerNode(qa,q,(if f then 2 else 0),i)) 
                                                                           duplicator_choices)
                                                        !lengths)
	 | InitialNode(qa,qb) -> List.map (fun i -> SpoilerNode(qa,qb,0,i)) !lengths

let make_pos qa qb = InitialNode(qa,qb)

(* let initial _ = InitialNode(initial !auta, initial !autb) *)

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
