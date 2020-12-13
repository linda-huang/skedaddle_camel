(** Computing and maintaining game score across levels *)

(** The type of all information relevant to computing score *)
type t = {
  mazes : int;
  time : float list; 
  hit : int;
  coins : int 
}

(** [time_mult] is the multiplier applied to the 
     time taken to complete a maze *)
val time_mult : float 

(** [update_score scorer time camel] is [scorer] updated with the completion 
    time of a level and the coins collected by [camel] in that level*)
val update_score : t -> float -> Camel.t -> t 

(** [score scr camel timed] is the score earned by [camel] 
    over the entire game, taking into account if the game 
    is [timed] or not *)
val score : t -> Camel.t -> bool -> int 

(** [string_of_score scr bool] is the nicely formatted numerical score, 
    health, and mazes completed *)
val string_of_score : t -> Camel.t -> bool -> string 

(** [init ()] creates an empty scorer *)
val init : unit -> t 