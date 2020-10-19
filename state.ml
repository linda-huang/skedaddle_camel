open Camel 
open Enemy
open Position
open Maze 
open Coin
open Random

type t = {
  camel : Camel.t;
  maze : Maze.maze;
  x_size : int;
  y_size : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  projectiles : Projectile.t list;
}

let camel_width = 20.
let tile_width = 50.
let near = 100.

(* [tile_to_pixel tx ty] is the (center) pixel location from [tx]th, [ty]th tile *)
let tile_to_pixel tx ty = 
  let f p = tile_width *. (p -. 1.) +. (tile_width /. 2.) in 
  (f tx, f ty)

(* TODO *)
(** [init_enemy_lst n] is an Array of [n] enemy camels with valid positions *)
let init_enemy_lst n = 
  (*let arr = Array.make n (Enemy.init 0 0 (make_pos 0. 0.)) in *)
  failwith "Unimplemented"

(* TODO *)
let init_coin_lst n =
  (*Array.make n (Coin.init 0 (valid_spawn_pos n))*)
  failwith "todo"


(* get a list of [n] unique valid positions to spawn an object *)
let valid_spawn_pos n mz = (* TODO: implement *)
  (*List.map tile_to_pixel (valid_spawn_pos_helper mz n [])*) failwith "todo"

(*TODO: implement*)
let rec valid_spawn_pos_helper mz n acc = 
  failwith "todo"
  (*
  if n = 0 then acc
  else (* currently does not prevent duplicate tiles *)
    valid_spawn_pos_helper mz (n - 1) (random_valid_tile mz) :: acc *)

(* try to generate a valid tile, return when found *)
let rec random_valid_tile mz = (* TODO make sure can access xsize and ysize of maze *)
  let x = Random.int (Array.length mz) in (* probably change how it's accessed later *)
  let y = Random.int (Array.length mz.(0)) in 
  if Maze.isWall mz x y then random_valid_tile mz else (x, y)

(*TODO: implement*)
(* returns a list of all non-wall tiles in [maze] *)
let valid_tile_list maze = [] 

(* get current tile number in maze from pixel location *)
let curr_tile pos = 
  let x = int_of_float (pos.x /. tile_width) in 
  let y = int_of_float (pos.y /. tile_width) in 
  (x, y)

(* detect if next move is a wall *)
let hit_wall (pos : Position.t) (maze : Maze.maze) = 
  let x = fst (curr_tile pos) in 
  let y = snd (curr_tile pos) in 
  Maze.isWall maze x y 

(* [near_enemy camel maze] detects if [camel]'s position is near 
   an enemy camel *)
let near_enemy (camel : Camel.t) (st : t) = 
  (* array filter, returns true if an enemy is within a certain distance *)
  let f (c : Enemy.t) = Position.dist c.pos camel.pos < near in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

(* [on_coin camel maze] detects if [camel]'s position is on a coin *)
let on_coin (camel : Camel.t) (st : t) = 
  Array.fold_left (fun acc x -> 
      (curr_tile (x.pos) = curr_tile camel.pos) || acc) false st.coins

(* [rem_coin] removes the selected coin from [st] *)
let rem_coin (c : Coin.t) (st : t) = failwith "todo"
(* {st with coins = Array.fold_left 
               (fun acc x -> if x = c then acc else x :: acc) Array st.coins} *)

(* [shoot camel] shoots a projectile in the direction of [camel]
   instantiates a new projectile in the state?? do we keep a list of all
   active projectiles as a field in the state*)
let shoot (camel : Camel.t) (st : t) = 
  let p = Projectile.init (List.length st.projectiles + 1) camel.dir camel.pos in 
  {st with projectiles = p :: st.projectiles} 

(* [move_proj st] is the state with all active projectiles moved one step 
   (e.g. in a straight line according to their direction). If a projectile runs
   into a wall, it stops and is removed from the game. *)
let move_proj (st : t) = 
  let st' = {st with projectiles = 
                       List.map Projectile.move_proj st.projectiles} in 
  {st' with projectiles = 
              List.filter (fun (p : Projectile.t) -> 
                  not (hit_wall p.pos st.maze)) st.projectiles}

(** [move_enemy enemy st] is [enemy] with updated position or direction.
    if [enemy] will hit a wall then it turns around, otherwise it
    keeps moving in the same direction. *)
let move_enemy (st : t) (enemy : Enemy.t) = 
  if hit_wall enemy.pos st.maze then (turn_around enemy)
  else move enemy

(** [move_enemies st] is the state after updating the position of all enemy
    camels. *)
let move_enemies (st : t) =
  {st with enemies = Array.map (move_enemy st) st.enemies}