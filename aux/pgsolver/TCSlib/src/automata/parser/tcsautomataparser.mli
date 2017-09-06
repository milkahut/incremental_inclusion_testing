open Tcsautomata

exception AutomataParserException of string * int * int

exception AutomataParserCustomException of string

val parse_parity_game: (int -> unit) -> (int -> unit) ->
                       (int -> int -> int -> int list -> string -> unit) ->
					   (unit -> 'a) ->
					   in_channel ->
					   'a

val parse_explicit_pg: in_channel -> explicit_pg
					   
val parse_explicit_initpg: in_channel -> explicit_initpg

val parse_parity_solution: (int -> unit) ->
                           (int -> int -> int option -> unit) ->
						   (unit -> 'a) ->
						   in_channel ->
						   'a

val parse_explicit_parity_solution: in_channel -> explicit_pg_solution * explicit_pg_strategy

val parse_lts: (int -> unit) -> (int -> unit) ->
               (int -> (string * int) list -> string list -> string option -> unit) ->
			   (unit -> 'a) ->
			   in_channel ->
			   'a

val parse_explicit_lts: in_channel -> explicit_lts
					   
val parse_explicit_initlts: in_channel -> explicit_initlts

val parse_ts: (int -> unit) -> (int -> unit) ->
              (int -> int list -> string list -> string option -> unit) ->
			  (unit -> 'a) ->
			  in_channel ->
			  'a

val parse_explicit_ts: in_channel -> explicit_ts
					   
val parse_explicit_initts: in_channel -> explicit_initts
