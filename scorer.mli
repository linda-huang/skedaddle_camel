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

(** [score scr camel] is the score earned by [camel] over the entire game *)
val score : t -> Camel.t -> int 

(** [string_of_score scr] is the nicely formatted numerical score, 
    health, and mazes completed *)
val string_of_score : t -> Camel.t -> string 

(** [init ()] creates an empty scorer *)
val init : unit -> t 