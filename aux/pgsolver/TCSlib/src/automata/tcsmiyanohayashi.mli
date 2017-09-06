open Tcsautomata

type 'a state = 'a list * 'a list

val miyano_hayashi: ('s, 'r) nba -> ('s state, 'r) dba
