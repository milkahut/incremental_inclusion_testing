(** VARIOUS UTILITY FUNCTIONS *)

(** {2 Debugging }*)

val debug: bool ref

val message: (unit -> string) -> unit


(** {2 Functions related with the Goal tool }*)

exception Goal_failure of string

val goal_absolute: unit -> string 
(** absolute path to call goal *)

(*
type aut_array = (int list array * bool) array
val goal_rand_nba: int->int->float->float->aut_array 
*)
(** generates a random nba calling Goal *) 
(** Usage : goal_rand_nba size alpha_size tr_density acc_density *)

(** {2 Time and timers }*) 

val pp_time:Format.formatter->float->unit
(** pretty-prints a time *)

exception Time_out
(** exception raised when a time-out is reached *)

val with_time_out: float->(unit->'a)->('a*float)
(** with_time_out t f starts evaluating f(): 
    - if it takes less than time t, then it returns (val,t') 
      where val is the result of f() and t' is the time it took 
      to compute it; 
    - otherwise, the exception Time_out is raised.
*)


val shell_with_time_out : float->string->string list
(** execs a shell command with some time-out, and returns the list
    of lines of the stdout of the command. 
    If Time_out is passed, it just return the list of lines 
    that it was possible to get before the timeout.
 *)

(** {2 Parallelisation }*)

val nbproc: int ref
(** the number of processors, or just the maximal number of parallelism.
When more tasks should execute in parallel, some tasks will wait for
a processor being done before being first scheduled. *)

val iter_in_parallel: ('a->unit)->'a list->unit
(** like List.iter, but in parallel *)

val do_in_parallel: (unit->unit) list -> unit
(** do_in_parallel [f1;..;fn] = f1() || ... || fn() *)  


(** {2 dat files } *)

type dat = (float * float) array
val read_dat: string->dat
val write_dat: dat->string->unit


(** {2 latex files} *)

val begin_document:Format.formatter->unit
val end_document:Format.formatter->unit
val pp_table:Format.formatter->(string array array)->unit

type color = string (** latex color, see xcolor manual *)
val blue:color
val red:color
val green:color
val brown:color
val pink:color

type mark=string (** mark used each point in a plot, see tikz manual *)
val dot:mark
val triangle:mark
val square:mark

type curve = {
    cu_name: string; (** name of the curve *)
    cu_fname: string; (** name of the file that should be plot *)
    cu_color: color; (** color of the curve *)
    cu_mark: mark; (** mark for a point *)
}


val pp_curves:Format.formatter->string->string->curve list->unit
(** USAGE: pp_curves fmt xlabel ylabel [curve_1;..;curve_n] *)
(** prints one graph with all curves compared *)


(** subfolders and files manipulation *)
val get_all_files: string->string list
val get_all_bench: string->string list
val get_subdir: string->string list
val get_foldername: string->string
