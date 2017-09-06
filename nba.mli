(** {2 Basics} *)


type state (** control states of NBA *)
type symbol  (** letters of NBA *)

val make_state: int -> state
val show_state: state  -> string
val show_symbol: symbol -> string


(** {2 NBAs }*)
type nba

(** {3 Basic functions } *)
val is_final: nba -> state -> bool
val initial: nba -> state
val alphabet_size: nba -> int
val size: nba -> int
val empty_nba: nba
val universal_nba: int  -> nba

val normalize_pair: (nba * nba) -> (nba * nba)
(** ensures that the two nba uses the same letter-int association list *)

(** {3 Functions computing the successor states} *)
val free_choices: nba -> state -> int -> (state * symbol list * bool) list (** free_choices aut q n is the list of pairs(q',w) such that q-w->q' and |w|=n *)
val guided_choices: nba -> state -> symbol list -> (state * bool) list 
(** guided_choices aut q w is the set of pairs (q,b) such that q-w->q' and if b then the path visits a final state *)



(** {2 Example NBAs} *)
(** {3 Example pairs of NBAs *)
val fooling_nbas: int  -> nba * nba
val tough_nbas: unit -> nba * nba
(** {3 randomization functions} *)
val random_nba: int -> int -> float -> float -> nba
(** Tabakov-Vardi model. 
    Usage : random_nba alphabetsize size tr_density acc_density *)
val state_splitting: nba -> int -> float -> float -> nba
(** state_splitting aut n x y is a nba accepting the same language as 
    aut obtained by n times splitting a state and its transitions 
    according to probabilities x and y *)


(** {2 parsing and printing} *)
val nba_of_gff_file: string -> nba (** parses a file in .gff format (Goal tool) *)
val gff_file_of_nba: nba->string -> unit (** outputs a nba into a .gff file *)

val nba_of_ba_file : string -> nba (** parses a file in ba format (languageinclusion.org files) *)
val ba_file_of_nba : nba -> string -> unit (** parses a file in ba format (languageinclusion.org files) *)

