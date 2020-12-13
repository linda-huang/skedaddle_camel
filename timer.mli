(** Timing level completion and related actions *)

(** The type of a timer *)
type timer = {
  starttime : float;
  elapsedtime : int 
} 

(** [time_left round timer] is the time remaining in [round].
    Used if the player chose to play the timed version of the game *)
val time_left : Constant.round_info -> timer -> int option

(** [out_of_time timer timelim] os if the time elapsed
    during a round exceeds [timelim] *)
val out_of_time : timer -> int -> bool 

(** [init_timer ()] is a fresh timer with
    the time at which a round began *)
val init_timer : unit -> timer 

(** [update_timer timer] updates [timer] to refer
    to the current time *)
val update_timer : timer -> timer 