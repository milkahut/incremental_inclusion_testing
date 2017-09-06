open Nba;;
open Paritygame;;


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

module MakeGame : functor (G: GameType) ->
sig
  val game: nba -> nba -> int -> (paritygame * (state -> state -> int))
end;;

