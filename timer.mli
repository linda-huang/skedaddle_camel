(** Timing level completion and related actions *)

(** The type of a timer *)
type timer = {
  starttime : float;
  elapsedtime : int; 
  totalpaused : float;
} 

(** [time_left curr_round st timer] is the time remaining in [round].
    Used if the player chose to play the timed version of the game *)
val time_left : Constant.round_info -> Round_state.t -> timer -> int option

(** [out_of_time curr_round st timer] is if the time elapsed
    during a round exceeds [timelim] *)
val out_of_time : Constant.round_info -> Round_state.t -> timer -> bool 

(** [init_timer ()] is a fresh timer with
    the time at which a round began *)
val init_timer : unit -> timer 

(** [update_timer timer] updates [timer] to refer
    to the current time, accounting for any time the game is paused *)
val update_timer : timer -> timer 

(** [string_of_timer timer] is the string representation of [timer] *)
val string_of_timer : timer -> string 