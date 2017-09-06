open Nba;;

type position = SpoilerNode of state * state * int
              | DuplicatorNode of state * state * int * (symbol list)
     	      | SpoilerTrap
              | DuplicatorTrap

let compare = compare

let auta = ref empty_nba
let autb = ref empty_nba
let k = ref 0 

let setup a b j = auta := a; autb := b; k := j

let owner = function SpoilerTrap             -> 0 
                   | DuplicatorTrap          -> 1
       	           | SpoilerNode(_,_,_)      -> 1
      	           | DuplicatorNode(_,_,_,_) -> 0

let priority = function SpoilerTrap             -> 0
                      | DuplicatorTrap          -> 1
      		      | SpoilerNode(_,_,p)      -> p
      		      | DuplicatorNode(_,_,p,_) -> p

let short_desc = function SpoilerTrap -> "1-trap"
                        | DuplicatorTrap -> "0-trap"
      	                | SpoilerNode(qa,qb,_) -> show_state qa ^ "," ^ show_state qb
      		        | DuplicatorNode(qa,qb,_,w) -> show_state qa ^ "," ^ show_state qb ^ "," ^ String.concat "" (List.map show_symbol w)

let long_desc = function SpoilerTrap -> "SpoilerTrap"
                       | DuplicatorTrap -> "DuplicatorTrap"
      	               | SpoilerNode(qa,qb,pr) -> "Sp(" ^ show_state qa ^ "," ^ show_state qb ^ "," ^ string_of_int pr ^ ")" 
      		       | DuplicatorNode(qa,qb,pr,w) -> "Du(" ^ show_state qa ^ "," ^ show_state qb ^ "," ^ string_of_int pr
                                                            ^ "," ^ String.concat "" (List.map show_symbol w) ^ ")"

let successors = function
           SpoilerTrap       -> [ SpoilerTrap ] 
         | DuplicatorTrap    -> [ DuplicatorTrap ] 
         | SpoilerNode(qa,qb,pr) -> let spoiler_choices = free_choices !auta qa !k in
                                    if spoiler_choices = [] then [ SpoilerTrap ]
                                    else List.map (fun (q,w,f) -> DuplicatorNode(q,qb,(if f then 1 else 0),w)) spoiler_choices 
         | DuplicatorNode(qa,qb,pr,w) -> let duplicator_choices = guided_choices !autb qb w in
                                         if duplicator_choices = [] then [ DuplicatorTrap ]
                                         else List.map (fun (q,f) -> SpoilerNode(qa,q,if f then 2 else 0)) duplicator_choices

let make_pos qa qb = SpoilerNode(qa,qb,0)

(* let initial _ = SpoilerNode(initial !auta, initial !autb, 0)  *)

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
