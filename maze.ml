open Graphics
open Constant 

(** The variant type indicating type of path tiles *)
type power_tile = Portal | Mud | Ice 

type t = 
  | Power_Path of power_tile
  | Wall of int
  | Path 
  | Exit
  | Start

type maze = t array array

(** [power_tiles] is a list of all types of power tiles *)
let power_tiles = [Mud; Ice]
(** [power_tile_len] is a list of lengths to support power tile generation *)
let power_tile_len = [1;2;3]

(** [in_limit maze row col] checks if the position corresponding to 
    [row] x [col] is within the bounds of [maze]. *)
let in_limit maze row col = (row >= 0) && (row < Array.length maze) && 
                            (col >= 0) && (col < Array.length maze.(0))

(** [power_or_not] is a weighted random generator that picks either power path
    or normal path. 
    The probability of selecting a power path to a normal path is 7:3. *)
let power_or_not () = 
  if (Random.int 10) < 7 then 0 else 1 

(** [rand_power_tile] generates a randomly picked power tile *)
let rand_power_tile () = 
  List.nth power_tiles (Random.int (List.length power_tiles))

(** [rand_power_len ()] is the number of power tiles of that type
    placed consecutively.*)
let rand_power_len () = 
  List.nth power_tile_len (Random.int (List.length power_tile_len))

(** [visited maze row col] is if the tile at [row] x [col] is a Wall *)
let visited maze row col = 
  if maze.(row).(col) = Wall 5 then false else true

(** [exchange arr i j] swaps the values at [arr.(i)] and [arr.(j)] *)
let exchange arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j);
  arr.(j) <- temp

(** [shuffle arr] shuffles values in [arr] *)
let shuffle arr = 
  let rec helper arr counter =
    Random.self_init ();
    if counter <> 0 then 
      let rand_i = Random.int (Array.length arr) in
      let rand_j = Random.int (Array.length arr) in
      exchange arr rand_i rand_j;
      helper arr (counter - 1)
    else ()
  in helper arr 4

(** [clear_path maze row col new_row new_col] creates a Path *)
let clear_path maze row col new_row new_col tile_type tile_len = 
  maze.(new_row).(new_col) <- tile_type;
  let diffx = new_row - row in
  let diffy = new_col - col in
  let next_tile_type = if tile_len = 1 then Path else tile_type in 
  if diffx = -2 then maze.(row-1).(col) <- next_tile_type
  else if diffx = 2 then maze.(row+1).(col) <- next_tile_type
  else if diffy = -2 then maze.(row).(col - 1) <- next_tile_type
  else if diffy = 2 then maze.(row).(col + 1) <- next_tile_type

(** [dfs maze row col] traverses the maze using depth first search
    [row] indicates the row number of the tile that the traversal is currently 
    visiting, [col] indicates the tile's column number *)
let rec dfs maze row col prev counter =
  let direction = [|(row - 2, col); (row + 2, col); 
                    (row, col - 2); (row, col + 2)|] in 
  shuffle direction;
  for i = 0 to Array.length direction - 1 do begin  
    let new_tile_type = if counter <= 0 then if power_or_not () = 0 then Path 
        else Power_Path (rand_power_tile ()) 
      else prev 
    in 
    let new_tile_len = if counter <= 0 then if new_tile_type = Path then 0 
        else rand_power_len ()
      else counter
    in 
    let new_row, new_col = direction.(i) in
    if in_limit maze new_row new_col && not (visited maze new_row new_col) 
    then begin
      clear_path maze row col new_row new_col new_tile_type new_tile_len;
      dfs maze new_row new_col new_tile_type (new_tile_len-2)
    end
    else ()
  end
  done

let populate cols rows start_pos = 
  let maze = Array.make_matrix rows cols (Wall Constant.wall_health) in
  let start_row, start_col = start_pos in
  dfs maze start_row start_col Path 0;
  maze.(start_row).(start_col) <- Path;
  maze.(0).(0) <- Start;
  maze.(rows-1).(cols-1) <- Exit;
  maze

let tile_type maze col row = 
  match maze.(row).(col) with
  | Exit -> Exit
  | Start -> Start
  | Power_Path x -> Power_Path x 
  | Wall x -> Wall x
  | Path -> Path
