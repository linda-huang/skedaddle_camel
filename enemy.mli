type t = {
  dir : int;
  pos : Position.t;
}

val change_dir : t -> int -> t 

(** [speed] is the number of pixels an Enemy moves per turn *)
val speed : int

(** [turn_around camel] is [camel] turned 180 degrees *)
val turn_around : t -> t

(** [move camel] is [camel] moved one move *)
val move : t -> t

(** [init d p] is a new Enemy with direction [d] and position [p] *)
val init : int -> Position.t -> t

(** [string_of_enemy e] is the string representation of [e] *)
val string_of_enemy : t -> string  