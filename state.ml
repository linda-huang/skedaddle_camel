open Camel 
open Enemy
open Position
open Maze 
open Coin

type t = {
  camel : Camel.t;
  maze : Maze.maze;
  enemies : Enemy.t array;
  coins : Coin.t array;
}

let camel_width = 20.
let tile_width = 50.
let near = 100.

(* get (center) pixel location from tile number in maze *)
let tile_to_pixel tx ty = 
  let f p = tile_width *. (p-.1) +. (tile_width /. 2.) in 
  (f tx, f ty)

(* instantiate an array of enemy camels *)
let enemy_lst n = 
  Array.make n (Enemy.init 0 0 (make_pos 0 0)) (* TODO: init enemy *)

let coin_lst n =
  Array.make n (Coin.init 0 (valid_spawn_pos) )

(* get a list of [n] unique valid positions to spawn an object *)
let valid_spawn_pos n mz = (* TODO: implement *)
  tile_to_pixel (valid_spawn_pos_helper (valid_tile_list mz) n [])

let rec valid_spawn_pos_helper avail_tiles n acc = (*TODO: implement*)
  if n = 0 then acc
  else
    valid_spawn_pos_helper avail_tiles (n - 1) acc 

(* returns a list of all non-wall tiles in [maze] *)
let valid_tile_list maze = [] (*TODO: implement*)

(* get current tile number in maze from pixel location *)
let curr_tile pos = 
  let x = int_of_float (pos.x /. tile_width) in 
  let y = int_of_float (pos.y /. tile_width) in 
  (x, y)

(* detect if next move is a wall *)
let hit_wall pos maze = 
  let x = fst (curr_tile pos) in 
  let y = snd (curr_tile pos) in 
  match maze.(x).(y) with 
  | Wall -> true 
  | _ -> false 

(* [near_enemy camel maze] detects if [camel]'s position is near 
   an enemy camel *)
let near_enemy camel st = 
  (* array filter, returns true if an enemy is within a certain distance *)
  let f c = Position.distance c camel.pos < near in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

(* [on_coin camel maze] detects if [camel]'s position is on a coin *)
let on_coin camel st = 
  Array.fold_left (fun acc x -> 
      (f (curr_tile (x.position) = curr_tile camel.pos) || acc)) false st.coins


