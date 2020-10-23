(** The abstract type representing the value of a tile in a maze. *)
type t = 
  | Wall
  | Path 
  | Exit
  | Start

(** The type of maze. *)
type maze = t array array

(** [populate maze m n (x, y)] is a randomly populated maze with m rows and 
    n columns. [(x, y)] is the index of the starting position.
    Requires:
    [m] >= 1
    [n] >= 1
*)
val populate : int -> int -> (int*int) -> maze

(** [isWall maze x y] is true when the maze.(y).(x) is a Wall. 
    Requires:
    0 <= [x] < number of columns of maze
    0 <= [y] < number of rows of maze. *)
val isWall : maze -> int -> int -> bool