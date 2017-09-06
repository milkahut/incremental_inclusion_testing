val goal_absolute : unit -> string
type aut_array = (int list array * bool) array

val aut_array_of_gff_file : string -> aut_array
val gff_file_of_aut_array : aut_array -> string -> unit

exception Goal_failure of string

val language_inclusion : aut_array->aut_array->bool

val expand_dot_dot_notation : string list -> string list

val goal_rand_nba : int->int->float->float->aut_array
