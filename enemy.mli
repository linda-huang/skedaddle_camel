(** Automated movement of enemy camels *)

(** The type of an enemy camel *)
type t = {
  dir : int;
  pos : Position.t;
}

(** [change_dir camel dir] is [camel] going in direction [dir].
    Requires: [rot] must be a multiple of 90 *)
val change_dir : t -> int -> t 

(** [move camel] is [camel] moved one step *)
val move : t -> t

(** [init d p] is a new Enemy with direction [d] and position [p] *)
val init : int -> Position.t -> t

(** [string_of_enemy e] is the string representation of [e] *)
val string_of_enemy : t -> string  