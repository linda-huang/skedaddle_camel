(** Random maze generation *)

(** The variant type indicating type of path tiles *)
type power_tile = Portal | Mud | Ice 

(** The abstract type representing the value of a tile in a maze. *)
type t = 
  | Power_Path of power_tile
  | Wall of int
  | Path 
  | Exit
  | Start

(** The type of maze *)
type maze = t array array

(** [populate maze cols rows (x, y)] is a randomly populated maze with [cols]
    columns and [rows] rows. [(x, y)] is the index of the starting position.
    Requires:
    [m] >= 1
    [n] >= 1
*)
val populate : int -> int -> (int * int) -> maze

(** [is_wall maze x y] is true when the maze.(y).(x) is a Wall. 
    Requires:
    0 <= [x] < number of columns of maze
    0 <= [y] < number of rows of maze. *)
val tile_type : maze -> int -> int -> t
