(** UTILITY FUNCTIONS FOR MANIPULATING BENCHMARKS, METHODS AND STATISTICS *)


(** {2 Benchmarks} *)
type benchmark_identifier = {
    bi_name: string; (** name of the (family of) benchmark *)
    bi_param: int option; (** parameter of the benchmark (e.g. : size) *)
    bi_variant: int option; (** variant of the benchmark *)
  }

val load_benchmark: benchmark_identifier->(Nba.nba*Nba.nba)
val save_benchmark: benchmark_identifier->Nba.nba->Nba.nba->unit



(** {2 Results} *)

type result
val no_result: result
val pp_result: Format.formatter->result->unit

type method_name = string

val get_res: benchmark_identifier->method_name->result
(** parses the file containing the result *)

val write_res: benchmark_identifier->method_name->result->unit
(** saves the result into the appropriate file *)

val inclusion_status:result->bool option
val construction_time:result->float option
val nb_iterations:result->int option

(* {2 Methods for solving language inclusion} *)

type timeout = float

val pg_solver: (Paritygame.paritygame->bool) ref
(** the function solving a parity game; 
    to be configured as desired
*)


type method_t = Nba.nba->Nba.nba->timeout->result->result
      (** usage: method aut1 aut2 timeout previous_res *)
      (** REMARK: if previous_res is already successful, or contains larger time-out, the method is not run *)

      
val m_static: method_t
val m_dynamic: method_t
val m_pebble: method_t

(* val m_goal: method_t *)
val m_arbait: method_t



