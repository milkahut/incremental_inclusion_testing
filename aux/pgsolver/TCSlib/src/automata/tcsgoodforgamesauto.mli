open Tcsautomata;;

type 'a state = ('a list * 'a list) list * int

val gfg_nba_to_npa: ('s, 'r) nba -> ('s state, 'r) npa
