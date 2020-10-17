open Camel 
open Enemy
open Position
open Maze 

type t = {
  camel : Camel.t;
  maze : Maze.t;
  enemies : Enemy.t array;
}

let camel_width = 20.
let tile_width = 50.

(* get pixel location from tile number in maze *)
let grid_to_pixel p = failwith "todo"

(* instantiate an array of enemy camels *)
let enemy_lst n = 
  Array.make n 0 (* TODO: init enemy *)

(* get current tile number in maze from pixel location *)
let curr_tile pos = 
  let x = int_of_float (pos.x /. tile_width) in 
  let y = int_of_float (pos.y /. tile_width) in 
  (x, y)

(* detect if next  move is a wall *)
let hit_wall pos maze = 
  let x = fst (curr_tile pos) in 
  let y = snd (curr_tile pos) in 
  match maze.(x).(y) with 
  | Wall -> true 
  | _ -> false 

(* detect if position is near an enemy camel *)
let near_enemy camel maze = 
  let x = (camel.pos).x in 
  let y = (camel.pos).y in 
  (* array filter, returns true if an enemy is within a certian distance *)
  let f c = 
    failwith "todo"
  in 
  Array.fold_left 
    (fun acc x -> (f x) || acc) false maze.enemies