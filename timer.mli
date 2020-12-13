(** Timing level completion and related actions *)

(** The type of a timer *)
type timer = {
  starttime : float;
  elapsedtime : int 
} 

(** [init_timer ()] is a fresh timer with
    the time at which a round began *)
val init_timer : unit -> timer 

(** [update_timer timer] updates [timer] to refer
    to the current time *)
val update_timer : timer -> timer 