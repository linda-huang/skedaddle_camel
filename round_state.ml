open Camel 
open Enemy
open Position
open Maze 
open Coin
open Random
open Constant

type t = {
  camel : Camel.t;
  maze : Maze.maze;
  cols : int;
  rows : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  projectiles : Projectile.t list;
  top_left_corner : int * int
}

(* *********************************************************
   interactions between various fields of round_state
   ********************************************************** *)
let on_coin (st : t)  = 
  Array.fold_left (fun acc (coin : Coin.t) -> 
      (Position.dist st.camel.pos coin.pos < 
       (Constant.camel_radius + Constant.coin_radius)) || acc)
    false st.coins

let at_exit (st : t) = 
  let camel = st.camel in 
  let coord_mapping = Position.pixel_to_tile camel.pos st.top_left_corner in
  match coord_mapping with 
  | Position.Out_of_bounds -> false
  | Position.Valid (col, row) -> Maze.tile_type st.maze col row = Exit 

(** [hit_corner st pos] detects if [pos] is out of bounds or is a wall*)
let hit_corner (st : t) (pos : Position.t) = 
  let coord_mapping = Position.pixel_to_tile pos st.top_left_corner in 
  match coord_mapping with 
  | Position.Out_of_bounds -> true
  | Position.Valid (col, row) -> col < 0 || row < 0 
                                 || row >= Array.length st.maze 
                                 || col >= Array.length st.maze.(0) 
                                 || (Maze.tile_type st.maze col row = Wall)

let hit_wall (st : t) (pos : Position.t) (dir : int) = 
  let tl = (pos.x - Constant.camel_radius, pos.y + Constant.camel_radius) in
  let tr = (pos.x + Constant.camel_radius, pos.y + Constant.camel_radius) in
  let bl = (pos.x - Constant.camel_radius, pos.y - Constant.camel_radius) in
  let br = (pos.x + Constant.camel_radius, pos.y - Constant.camel_radius) in
  let two_corners =
    match dir with 
    | 0 -> (tr, br)
    | 90 -> (tl, tr)
    | 180 -> (tl, bl)
    | 270 -> (bl, br)
    | _ -> failwith "impossible"
  in hit_corner st (Position.init_pos (fst two_corners)) || hit_corner st 
       (Position.init_pos (snd two_corners))

(**********************************************************
   helpers for updating round_state
 ***********************************************************)
(* [random_valid_tile mz] is a random valid (non-wall) tile in [mz] *)
let rec random_valid_tile mz = (* TODO make sure can access xsize and ysize of maze.*)
  let col = Random.int (Array.length mz - 1) in
  let row = Random.int (Array.length mz.(0) - 1) in 
  if Wall = Maze.tile_type mz col row then random_valid_tile mz else (col, row)

(** [random_valid_tile_enemy mz] is a random valid (non-wall) tile in [mz]
    that is at least 5 tiles away from the origin *)
let rec random_valid_tile_enemy mz = 
  let (x, y) = random_valid_tile mz in 
  if x < 5 || y < 5 then random_valid_tile_enemy mz else (x, y)

(* [near_enemy camel maze] detects if [camel]'s position is near 
   an enemy camel *)
let near_enemy (camel : Camel.t) (st : t) = 
  (* array filter, returns true if an enemy is within a certain distance *)
  let f (c : Enemy.t) = Position.dist c.pos camel.pos < near in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

(* [shoot camel] shoots a projectile in the direction of [camel]
   instantiates a new projectile in the round_? do we keep a list of all
   active projectiles as a field in the round_*)
let shoot (camel : Camel.t) (st : t) = 
  let p = Projectile.init camel.dir camel.pos 
  in {st with projectiles = p :: st.projectiles} 

(* [move_proj st] is the round_with all active projectiles moved one step 
   (e.g. in a straight line according to their direction). If a projectile runs
   into a wall, it stops and is removed from the game. *)
let move_proj (st : t) = 
  let st' = {st with projectiles = 
                       List.map Projectile.move_proj st.projectiles} in 
  {st' with projectiles = 
              List.filter (fun (p : Projectile.t) -> 
                  not (hit_wall st' p.pos p.dir)) st'.projectiles}

(** [hit_enemy st] checks if any projectiles in [st] have hit an enemy. 
    If a projectile has hit an enemy, both the projectile and enemy 
    are removed from [st] *)
let hit_enemy (st : t) = 
  let rec check_proj (lst : Projectile.t list) 
      ((accproj : Projectile.t list), (accenemy : Enemy.t list)) = 
    match lst with 
    | [] -> (accproj, accenemy)
    | h :: t -> let remaining  = 
                  List.fold_left (fun acc (x : Enemy.t) -> 
                      if Position.dist x.pos h.pos < near + camel_width  
                      then acc else x :: acc) [] accenemy in 
      if List.length remaining = List.length accenemy  
      then check_proj t (h :: accproj, remaining) 
      else check_proj t (accproj, remaining)
  in   
  let (newproj, newenemy) = 
    check_proj st.projectiles ([], Array.to_list st.enemies) in 
  {st with enemies = Array.of_list newenemy; projectiles = newproj}

(** [move_enemy enemy st] is [enemy] with updated position or direction.
    if [enemy] will hit a wall then it turns around, otherwise it
    keeps moving in the same direction. *)
let move_enemy (st : t) (enemy : Enemy.t) : Enemy.t = 
  let future_pos = (move enemy).pos in 
  if not (hit_wall st future_pos enemy.dir) then move enemy
  else 
    let next_move_l = Enemy.change_dir enemy 180 in
    let next_move_r = Enemy.change_dir enemy 0 in
    let next_move_up = Enemy.change_dir enemy 90 in
    let next_move_down = Enemy.change_dir enemy 270 in 
    let all_moves = [next_move_l; next_move_r; next_move_up; next_move_down] in
    let valid_moves = List.filter (fun next_move -> not (hit_wall st 
                                                           (move next_move).pos next_move.dir)) 
        all_moves in 
    let random_turn_enemy =  List.nth valid_moves (Random.int (List.length valid_moves))
    in 
    move (random_turn_enemy) 


(** [move_enemies st] is [st] with all enemies moved one move *)
let move_enemies (st : t) : t =
  {st with enemies = Array.map (move_enemy st) st.enemies}

(* [update_camel st] is the round_with the camel's 
   health and coin total updated *)
let update_camel (st : t) : t = 
  let camel = st.camel in 
  let camel' = if (near_enemy camel st) then 
      {camel with health = camel.health - 1} else camel in 
  (* let camel'' = if (on_coin st) then 
      {camel with coins = camel.coins + 1} else camel' in  *)
  {st with camel = camel'}  

(* [remove_coin c st] is [st] with [c] removed *)
let remove_coin (c : Coin.t) (st : t) = 
  let coinlst = Array.fold_left 
      (fun acc x -> if x = c then acc else x :: acc) [] st.coins in 
  {st with coins = Array.of_list coinlst}

(** [get_coin st] is [st] with the coin the camel is currently on removed *)
let get_coin (st : t) : t = 
  let c = Coin.find_coin st.camel.pos st.coins in 
  let st' = remove_coin c st in 
  {st' with camel = {st'.camel with coins = st'.camel.coins + c.value}} 

(** [update round_st] is [st] with all agents updated one move
    e.g. all enemies moved one step; projectiles moved one unit; 
    any applicable coins picked up; camel score and health adjusted *)
let update_round_state (st : t) : t = 
  let st' = update_camel st |> move_proj |> move_enemies |> hit_enemy in 
  if on_coin st' then get_coin st' else st' 

(************************************************************
   initialization
 ***********************************************************)
(** [init_enemy_lst n mz] is an Array of [n] enemy camels with valid positions *)
let init_enemy_lst (n : int) (mz : Maze.maze) (start_pos): Enemy.t array = 
  Array.init n (fun i -> 
      (Enemy.init (90 * Random.int 4) 
         (random_valid_tile mz |> Position.tile_to_pixel start_pos 
          |> Position.init_pos)))

(** [init_coin_lst n mz] is an Array of [n] coins with valid positions in [mz]. *)
let init_coin_lst n mz start_pos=
  Array.init n (fun i -> 
      (Coin.init 
         (random_valid_tile mz |> Position.tile_to_pixel start_pos 
          |> Position.init_pos) (100 * Random.int 4 + 100)))

(** [init camel x y numenemy] is a fresh round_with [camel] at
    the beginning of an [x] x [y] maze with [numenemy] enemies *)
let init rows cols numenemy = 
  let mz = Maze.populate rows cols (0,0) in 
  let maze_row = rows in
  let maze_col = cols in
  let window_height = maze_row * Constant.tile_width + 200 in 
  let window_width = maze_col * Constant.tile_width + 200 in
  let start_y = window_height - ((window_height- maze_row * Constant.tile_width) / 2) in
  let start_x = ((window_width - maze_col * Constant.tile_width) / 2) in
  let start_pos = (start_x, start_y) in
  let camel = Camel.init ((fst start_pos) + camel_radius) ((snd start_pos) - camel_radius) 
  in
  Graphics.resize_window window_width window_height;
  {camel = camel; 
   maze = mz;
   cols = cols;
   rows = rows;
   enemies = init_enemy_lst numenemy mz start_pos;
   coins = init_coin_lst 20 mz start_pos;
   projectiles = [];
   top_left_corner = start_pos}

(**********************************************************
   pretty printing things
 ***********************************************************)
(** [pp_array arr f] is a nicely formatted string of [arr] with 
    each elementn formatted according to [f]. * *)
let pp_array arr f = 
  Array.fold_left (fun acc x -> f x ^ ", " ^ acc) "" arr 

(** [pp_lst lst f] is a nicely formatted string of [lst] with 
    each elementn formatted according to [f]. *)
let pp_lst lst f = 
  List.fold_left (fun acc x -> f x ^ ", " ^ acc) "" lst 

let string_of_round_state st = 
  "Camel: " ^ Camel.string_of_camel st.camel ^ 
  "\n" ^ "Enemies: " ^ pp_array st.enemies Enemy.string_of_enemy ^ 
  "\n" ^  "Coins: " ^ pp_array st.coins Coin.string_of_coin ^ 
  "\n" ^  "Projectiles: " ^ pp_lst st.projectiles Projectile.string_of_proj
