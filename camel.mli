(** The player camel movement and information updating *)
open Position 
open Constant
open Hourglass

(** The type of a player camel.
    [pos] is the center coordinates of the camel; 
    [dir] is the direction in degrees;
    [lasthealthlost] is the last time the camel lost health; 
    [hourglasses] is the power of any hourglass a camel may have collected *)
type t = {
  pos : Position.t;
  dir : int; (* direction in degrees *)
  health : int;
  lasthealthlost : float; 
  coins : int;
  speed : int;
  last_tile : Maze.t;
  teleport : bool;
  hourglasses : hourglass_power option;
  ice_goal : (int * int * int) option;
}

(** [init x y] is a new camel at position ([x],[y]) *)
val init : int -> int -> t 

(** [change_dir camel dir] is [camel] going in direction [dir].
    Requires: [rot] must be a multiple of 90 *)
val change_dir : t -> int -> t 

(** [turn_right camel] is [camel] turned right [rot] degrees.
    Requires: [rot] must be a multiple of 90 *)
val turn_right : t -> t 

(** [turn_left camel] is [camel] turned right [rot] degrees.
    Requires: [rot] must be a multiple of 90 *)
val turn_left : t -> t 

(** [move_horiz camel sign key] is [camel] after moving horizontally 
    one step. The direction of movement (left/right) is determined by [sign]
    and direction is determined by [key]
    Requires: [sign] is either ~-1. or 1. *)
val move_horiz : t -> int -> char -> t 

(** [move_vert camel sign key] is [camel] after moving vertically 
    one step. The direction of movement (up/down) is determined by [sign]
    and direction is determined by [key]
    Requires: [sign] is either ~-1. or 1. *)
val move_vert : t -> int -> char -> t

(** [move camel] is [camel] moved one step *)
val move : t -> t

(** [teleport camel pos] is [camel] teleported to [pos] *)
val teleport : t -> Position.t -> t

(** [adj_health camel h] is [camel] with health 
    incremented/decremented by [h]. The camel's health cannot go
    below 0 or exceed [Constant.num_lives] *)
val adj_health : t -> int -> t

(** [is_dead camel] is if [camel] has run out of health *)
val is_dead : t -> bool

(** [string_of_camel camel] is the string representation of [camel] *)
val string_of_camel : t -> string 
