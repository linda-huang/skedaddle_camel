(* denotes whether the position is a wall, a path, or an exit path *)
type t = 
  | Wall
  | Path 
  | Exit Path

(* maze as a 2d array?*)
type maze = t array array

(* Some sort of way to keep the valid path?? *)
type exit_path = {
  value : int*int;
  next : path;
}

(* start pos is top left corner??*)
let start_pos = (0, 0)

(* exit is on lower right corner??*)
let exit x y = (x - 1, y - 1)

(* [populate x y] populates a random maze of size [x] by [y] with
   entry at (0, 0) and exit at (x - 1, y - 1)
   Requires:
   [x] >= 1
   [y] >= 1
*)
let populate x y = 
  failwith "Unimplemented"



