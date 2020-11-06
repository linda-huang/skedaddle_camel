(* the type [t] stores all information relevant to scoring *)
type t = {
  mazes : int;
  time : float list; 
}

(**  [time_mult] is the multiplier applied to the 
     time taken to complete a maze *)
val time_mult : float 

(** [update_time scorer] is [scorer] updated with the 
    completion time of a new maze *)
val update_time : t -> float -> t 

(** [score scr camel] is the score earned by [camel] over the entire game *)
val score : t -> Camel.t -> int 

(** [string_of_score scr] is the nicely formatted numerical  core, 
    health, and mazes completed *)
val string_of_score : t -> Camel.t -> string 

(** [init ()] creates an empty scorer *)
val init : unit -> t 