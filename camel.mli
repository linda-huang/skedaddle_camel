(** The player camel movement and information updating *)

(** The type of a player camel.
    [pos] is the center coordinates of the camel; 
    [dir] is the direction in degrees *)
type t = {
  pos : Position.t;
  dir : int; (* direction in degrees *)
  health : int;
  lasthealthlost : float; 
  coins : int;
}

(** [init x y] is a new camel at position ([x],[y]) *)
val init : int -> int -> t 

(** [turn_right camel] is [camel] turned right [rot] degrees.
    Requires: [rot] must be a multiple of 90 *)
val turn_right : t -> t 

(** [turn_left camel] is [camel] turned right [rot] degrees.
    Requires: [rot] must be a multiple of 90 *)
val turn_left : t -> t 

(** [move_horix camel sign] is [camel] after moving horizontally 
    one step. The direction of movement (left/right) is determined by [sign]
    Requires: [sign] is either ~-1. or 1. *)
val move_horiz : t -> int -> char -> t 

(** [move_vert camel sign] is [camel] after moving vertically 
    one step. The direction of movement (up/down) is determined by [sign]
    Requires: [sign] is either ~-1. or 1. *)
val move_vert : t -> int -> char -> t

(** [adj_health camel h] is [camel] with health 
    incremented/decremented by [h] *)
val adj_health : t -> int -> t

(** [adj_coin camel v] is [camel] after picking up a coin of value [v] *)
val adj_coin : t -> int -> t

(** [is_dead camel] is if [camel] has run out of health *)
val is_dead : t -> bool

(** [string_of_camel camel] is the string representation of [camel] *)
val string_of_camel : t -> string 
