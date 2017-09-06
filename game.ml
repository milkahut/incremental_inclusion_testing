open Misc;;
open Nba;;
open Paritygame;;

(* type priority = int *)

module type GameType = 
sig
  type pos

  val compare: pos -> pos -> int

  val setup : nba -> nba -> int -> unit

  val successors: pos -> pos list
  val owner     : pos -> int
  val priority  : pos -> int
  val short_desc: pos -> string
  val long_desc : pos -> string
  val initials  : unit     -> pos list
  val make_pos  : state -> state -> pos
end ;;

module MakeGame = functor (G: GameType) -> 
struct

  module PositionCoding = Map.Make(struct
    type t = G.pos
    let compare = G.compare
  end);;
  module IntSet = Set.Make(struct 
    type t = int
    let compare = compare
  end);;


  let game auta autb k = 
    let positions = ref PositionCoding.empty in
    let counter = ref 0 in
 
    let encode p = 
      try 
        PositionCoding.find p !positions 
      with
        Not_found -> begin
                       let c = !counter in 
                       positions := PositionCoding.add p c !positions;
                       incr counter;
                       c
                     end
    in

  G.setup auta autb k;

  let todo = ref (G.initials ()) in
  let visited = ref IntSet.empty in
  let game = ref [] in

  while !todo <> [] do
    message (fun _ -> "GAME: starting new iteration; length of todo list: " ^ string_of_int (List.length !todo));
    let p = List.hd !todo in
    todo := List.tl !todo;
    message (fun _ -> "  Node to process: " ^ G.long_desc p);
    let c = encode p in

    if not (IntSet.mem c !visited) then
      begin
        message (fun _ -> "  Node is new. Processing.");
        let ps = G.successors p in 
        game := (c, G.priority p, G.owner p, List.map encode ps, G.short_desc p) :: !game;
        todo := List.append ps !todo;
        visited := IntSet.add c !visited 
      end
    else
      message (fun _ -> "  Node has been seen before. Skipping.")
  done;

  let paritygame = pg_create !counter in
  List.iter (fun (c,pr,ow,ss,nm) -> pg_set_node paritygame c pr ow (Array.of_list ss) (Some nm)) !game;
  let encoder qa qb = let p = G.make_pos qa qb in
                      try 
                         PositionCoding.find p !positions 
                      with
                         Not_found -> failwith "Game.game.encoder: unknown position!\n"
  in                      
  (paritygame,encoder)

end;;

