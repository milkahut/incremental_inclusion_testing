open Tcstiming
open Tcsbasedata


module NonDetInfWordAut : sig

	type ('s, 'r, 'a) automaton =
		('s, 'r, 'a) BaseType.t3 *
		's * ('s -> 'r -> 's Iterators.iterator) * ('s -> 'a)
		
	type ('s, 'r) nba = ('s, 'r, bool) automaton
	
	type ('s, 'r) npa = ('s, 'r, int) automaton
	
	type 's total_state = Default of 's | Stuck of 's
	
	
	val make_total: ('s, 'r, 'a) automaton -> ('s -> 'a) -> ('s total_state, 'r, 'a) automaton
	val nba_make_total: ('s, 'r) nba -> ('s total_state, 'r) nba
	val npa_make_total: ('s, 'r) npa -> ('s total_state, 'r) npa
	
	val profile_time: ('s, 'r, 'a) automaton ->
		SimpleTiming.timing_object -> ('s, 'r, 'a) automaton
		
	val change_alphabet: ('s, 'r, 'a) automaton -> ('t, 'r) BaseType.mapping -> ('s, 't, 'a) automaton
	
	val change_states: ('s, 'r, 'a) automaton -> ('t, 's) BaseType.identity -> ('t, 'r, 'a) automaton
	
	val cache: ('s, 'r, 'a) automaton -> ('s, 'a) Mapping.dynmap ->
	           ('s * 'r, 's Iterators.iterator) Mapping.dynmap -> ('s, 'r, 'a) automaton
			   
end


module DetInfWordAut : sig

	type ('s, 'r, 'a) automaton =
		('s, 'r, 'a) BaseType.t3 *
		's * ('s -> 'r -> 's) * ('s -> 'a)
		
	type ('s, 'r) dba = ('s, 'r, bool) automaton
	
	type ('s, 'r) dpa = ('s, 'r, int) automaton
	
	val profile_time: ('s, 'r, 'a) automaton ->
		SimpleTiming.timing_object -> ('s, 'r, 'a) automaton
		
	val change_alphabet: ('s, 'r, 'a) automaton -> ('t, 'r) BaseType.mapping -> ('s, 't, 'a) automaton
	
	val change_states: ('s, 'r, 'a) automaton -> ('t, 's) BaseType.identity -> ('t, 'r, 'a) automaton
	
	val cache: ('s, 'r, 'a) automaton -> ('s, 'a) Mapping.dynmap ->
	           ('s * 'r, 's) Mapping.dynmap -> ('s, 'r, 'a) automaton
			   
end


type ('s, 'r) nba = ('s *
                     ('s -> 'r -> 's list) *
                     ('s -> bool)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

val get_timed_nba :
  ('s, 'r) nba -> SimpleTiming.timing_object -> ('s, 'r) nba

val get_cached_nba :
  ('s, 'r) nba -> ('s, 'r) nba

val get_int_cached_nba :
  ('s, 'r) nba -> (('s -> int -> unit) * (('s * 'r) -> int -> unit)) -> (int, 'r) nba

val nba_switch_final: ('s, 'r) nba -> ('s, 'r) nba
					 
type ('s, 'r) dba = ('s *
                     ('s -> 'r -> 's) *
                     ('s -> bool)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

val get_timed_dba :
  ('s, 'r) dba -> SimpleTiming.timing_object -> ('s, 'r) dba

val get_cached_dba :
  ('s, 'r) dba -> ('s, 'r) dba

val get_int_cached_dba :
  ('s, 'r) dba -> (('s -> int -> unit) * (('s * 'r) -> int -> unit)) -> (int, 'r) dba

type ('s, 'r) npa = ('s *
                     ('s -> 'r -> 's list) *
                     ('s -> int)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

val get_timed_npa :
  ('s, 'r) npa -> SimpleTiming.timing_object -> ('s, 'r) npa

val get_cached_npa :
  ('s, 'r) npa -> ('s, 'r) npa

val get_int_cached_npa :
  ('s, 'r) npa -> (('s -> int -> unit) * (('s * 'r) -> int -> unit)) -> (int, 'r) npa

type ('s, 'r) dpa = ('s *
                     ('s -> 'r -> 's) *
                     ('s -> int)) *
                    (int option *
                     ('s -> 's -> int) option *
                     ('r -> 'r -> int) option) *
                    (('s -> string) *
                     ('r -> string))

val trivial_dba_to_dpa: ('s, 'r) dba -> int -> int -> ('s, 'r) dpa

val trivial_dpa_to_npa: ('s, 'r) dpa -> ('s, 'r) npa

val get_timed_dpa :
  ('s, 'r) dpa -> SimpleTiming.timing_object -> ('s, 'r) dpa

val get_cached_dpa :
  ('s, 'r) dpa -> ('s, 'r) dpa

val get_int_cached_dpa :
  ('s, 'r) dpa -> (('s -> int -> unit) * (('s * 'r) -> int -> unit)) -> (int, 'r) dpa

type 's initpg = ('s *
                  ('s -> 's list) *
                  ('s -> int) *
                  ('s -> bool) *
                  ('s -> string)) *
                 (int option *
                  int option *
                  ('s -> 's -> int) option)
				  
type 's initpg_solution = 's -> bool option

type 's initpg_strategy = 's -> 's option				  
				  
val get_compact_initpg :
  's initpg -> ('s -> bool) -> ('s * int) initpg
  
val get_compact_initpg_by_player :
  's initpg -> bool -> ('s * int) initpg

val get_escaped_initpg :
  's initpg -> 'a -> ('s * 'a) initpg
  
val get_timed_initpg :
  's initpg -> SimpleTiming.timing_object -> 's initpg

val get_cached_initpg :
  's initpg -> 's initpg

val get_int_cached_initpg :
  's initpg -> ('s -> int -> unit) ->
  int initpg * ('s -> int) * (int -> 's)

type explicit_pg = (int * int * int array * string) array

type explicit_initpg = int * explicit_pg

val build_explicit_initpg : int initpg -> (int -> unit) -> explicit_initpg

val print_explicit_pg: explicit_pg -> (string -> unit) -> unit

val print_explicit_initpg: explicit_initpg -> (string -> unit) -> unit

type explicit_pg_solution = int array

type explicit_pg_strategy = int array


type ('s, 'l, 'p) initlts =
	('s *
	 ('s -> 'p list) *
	 ('s -> ('l * 's) list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('l -> string) *
	 ('p -> string))

val get_timed_initlts:
	('s, 'l, 'p) initlts -> SimpleTiming.timing_object -> ('s, 'l, 'p) initlts

val get_cached_initlts:
	('s, 'l, 'p) initlts -> ('s, 'l, 'p) initlts

val get_int_cached_initlts :
  ('s, 'l, 'p) initlts -> ('s -> int -> unit) ->
  (int, 'l, 'p) initlts * ('s -> int) * (int -> 's)

type explicit_lts =
	string array * (* propositions *)
	string array * (* labels *)
	(int array * (* propositions *)
	 (int * int) array * (* label,node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initlts = int * explicit_lts

val build_explicit_initlts:
	(int, 'l, 'p) initlts -> (int -> 's) -> ('s -> string option) -> (int -> unit) -> explicit_initlts

val print_explicit_lts: explicit_lts -> (string -> unit) -> unit

val print_explicit_initlts: explicit_initlts -> (string -> unit) -> unit

type ('s, 'p) initts =
	('s *
	 ('s -> 'p list) *
	 ('s -> 's list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('p -> string))

val get_timed_initts:
	('s, 'p) initts -> SimpleTiming.timing_object -> ('s, 'p) initts

val get_cached_initts:
	('s, 'p) initts -> ('s, 'p) initts

val get_int_cached_initts :
  ('s, 'p) initts -> ('s -> int -> unit) ->
  (int, 'p) initts * ('s -> int) * (int -> 's)

type explicit_ts =
	string array * (* propositions *)
	(int array * (* propositions *)
	 int array * (* node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initts = int * explicit_ts

val build_explicit_initts:
	(int, 'p) initts -> (int -> 's) -> ('s -> string option) -> (int -> unit) -> explicit_initts

val print_explicit_ts: explicit_ts -> (string -> unit) -> unit

val print_explicit_initts: explicit_initts -> (string -> unit) -> unit
