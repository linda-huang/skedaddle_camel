
(** denotes whether the position is a wall, a path, or an exit, or start*)

open Graphics
open Constant 

(* denotes whether the position is a wall, a path, or an exit, or start*)

type t = 
  | Wall
  | Path 
  | Exit
  | Start

(* maze as a 2d array*)
type maze = t array array

let in_limit maze row col = (row >= 0) && (row < Array.length maze) && 
                            (col >= 0) && (col < Array.length maze.(0))
let fightingring : maze = 
  let arr = Array.init 30 (fun i -> Array.init 30 (fun i -> Path)) in 
  (arr.(0)).(0) <- Start;
  (arr.(29)).(29) <- Exit;
  arr

let visited maze row col = 
  if maze.(row).(col) = Wall then false else true

let exchange arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j);
  arr.(j) <- temp

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

let clear_path maze row col new_row new_col = 
  maze.(new_row).(new_col) <- Path;
  let diffx = new_row - row in
  let diffy = new_col - col in
  if diffx = -2 then maze.(row-1).(col) <- Path
  else if diffx = 2 then maze.(row+1).(col) <- Path
  else if diffy = -2 then maze.(row).(col - 1) <- Path
  else if diffy = 2 then maze.(row).(col + 1) <- Path

let rec dfs maze row col =
  let direction = [|(row - 2, col); (row + 2, col); 
                    (row, col - 2); (row, col + 2)|] in 
  shuffle direction;
  for i = 0 to Array.length direction - 1 do begin
    let new_row, new_col = direction.(i) in
    if in_limit maze new_row new_col && not (visited maze new_row new_col) 
    then begin
      clear_path maze row col new_row new_col;
      dfs maze new_row new_col
    end
    else ()
  end
  done

let populate cols rows start_pos = 
  let maze = Array.make_matrix rows cols Wall in
  let start_row, start_col = start_pos in
  dfs maze start_row start_col;
  maze.(start_row).(start_col) <- Path;
  maze.(0).(0) <- Start;
  maze.(rows-1).(cols-1) <- Exit;
  maze

let tile_type maze col row = 
  match maze.(row).(col) with
  | Exit -> Exit
  | Start -> Start
  | Wall -> Wall
  | Path -> Path

