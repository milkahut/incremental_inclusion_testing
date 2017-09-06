open Tcsautomata;;

type 'a ctree = Node of int * 'a list * 'a ctree list
type 'a state = 'a ctree * int

val piterman_nba_to_dpa: ('s, 'r) nba -> ('s state, 'r) dpa