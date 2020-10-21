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
  (float_of_int tx |> f, float_of_int ty |> f)

let tile_to_pixel_2 tuple = 
  let f p = tile_width *. (p -. 1.) +. (tile_width /. 2.) in 
  (fst tuple|> float_of_int |> f, snd tuple |> float_of_int |> f)

(* try to generate a valid tile, return when found *)
let rec random_valid_tile mz = (* TODO make sure can access xsize and ysize of maze *)
  let x = Random.int (Array.length mz) in
  let y = Random.int (Array.length mz.(0)) in 
  if Maze.isWall mz x y then random_valid_tile mz else (x, y)

(* get an array of [n] unique valid positions to spawn an object
   let valid_spawn_pos (n : int) mz = (* TODO: implement *)
   let tile = random_valid_tile mz in
   Array.init n (Position.make_pos tile.x tile.y) *)

let random_direction = Random.int (4) * 90

(** [init_enemy_lst n] is an Array of [n] enemy camels with valid positions *)
let init_enemy_lst (n : int) (mz : Maze.maze) : Enemy.t array = 
  Array.init n (fun i -> (Enemy.init random_direction (random_valid_tile mz |> tile_to_pixel_2 |> make_pos_2)))

let init_coin_lst n mz =
  Array.init n (fun i -> (Coin.init (random_valid_tile mz |> tile_to_pixel_2 |> make_pos_2) 100))

(* get current tile number in maze from pixel location *)
let curr_tile pos = 
  let x = int_of_float (pos.x /. tile_width) in 
  let y = int_of_float (pos.y /. tile_width) in 
  (x, y)

(* [hit_wall pos maze] detect if the position is a valid
   move in [maze] *)
let hit_wall (pos : Position.t) (maze : Maze.maze) = 
  let (x, y) = curr_tile pos in 
  pos.x < 0. || pos.y < 0. || Maze.isWall maze x y  

(* [near_enemy camel maze] detects if [camel]'s position is near 
   an enemy camel *)
let near_enemy (camel : Camel.t) (st : t) = 
  (* array filter, returns true if an enemy is within a certain distance *)
  let f (c : Enemy.t) = Position.dist c.pos camel.pos < near in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

(* [on_coin camel maze] detects if [camel]'s position is on a coin *)
let on_coin (camel : Camel.t) (st : t) = 
  Array.fold_left (fun acc (x : Coin.t) -> 
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
let move_enemy (st : t) (enemy : Enemy.t) : Enemy.t = 
  if hit_wall enemy.pos st.maze then (Enemy.turn_around enemy)
  else move enemy

(** [move_enemies st] is the state after updating the position of all enemy
    camels. *)
let move_enemies (st : t) : t =
  {st with enemies = Array.map (move_enemy st) st.enemies}

(** [init camel x y numenemy] is a fresh state with [camel] at
    the beginning of an [x] x [y] maze with [numenemy] enemies *)
let init camel x y numenemy = 
  let mz = Maze.populate x y (0,0) in 
  {camel = camel; 
   maze = mz;
   x_size = x;
   y_size = y;
   enemies = init_enemy_lst numenemy mz;
   coins = init_coin_lst 20 mz;
   projectiles = []}


let pp_array arr f = 
  Array.fold_left (fun acc x -> f x ^ ", " ^ acc) "" arr 

let pp_lst lst f = 
  List.fold_left (fun acc x -> f x ^ ", " ^ acc) "" lst 

let string_of_state st = 
  "Camel: " ^ Camel.string_of_camel st.camel ^ 
  "\n" ^ "Enemies: " ^ pp_array st.enemies Enemy.string_of_enemy ^ 
  "\n" ^  "Coins: " ^ pp_array st.coins Coin.string_of_coin ^ 
  "\n" ^  "Projectiles: " ^ pp_lst st.projectiles Projectile.string_of_proj