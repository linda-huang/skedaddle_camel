open Camel 
open Enemy
open Position
open Maze 
open Coin
open Random
open Constant
open Potion
open Genie
open Hourglass 

type t = {
  camel : Camel.t;
  maze : Maze.maze;
  cols : int;
  rows : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  potions : Potion.potion array;
  projectiles : Projectile.t list;
  genie : Genie.genie option;
  hourglass : Hourglass.hourglass option;
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

let on_potion (st : t) = 
  Array.fold_left (fun acc (potion : Potion.potion) -> 
      (Position.dist st.camel.pos potion.pos < 
       (Constant.camel_radius + Constant.potion_radius)) || acc)
    false st.potions

(** [on_hourglass camel st] detects if the position of [camel] in [st] 
    is on an hourglass. *)
let on_hourglass (camel : Camel.t) (st : t) = 
  match st.hourglass with 
  | None -> false
  | Some hourglass -> begin 
      if Position.dist hourglass.pos camel.pos < camel_width 
      then true else false 
    end 

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
(** [random_valid_tile mz] is a random valid (non-wall) tile in [mz] *)
(* TODO make sure can access xsize and ysize of maze.*)
let rec random_valid_tile mz = 
  let row = Random.int (Array.length mz - 1) in
  let col = Random.int (Array.length mz.(0) - 1) in 
  if Wall = Maze.tile_type mz col row 
  then random_valid_tile mz else (col, row)

(** [random_valid_tile_potion mz start_pos coins] makes sure the tile 
    a new potion is generated on does not already have a coin on it *)
let rec random_valid_tile_potion mz start_pos coins = 
  let c, r = random_valid_tile mz in 
  let potionpos = (tile_to_pixel start_pos (c, r)) 
                  |> Position.init_pos in 
  let occupied = Array.fold_left (fun acc (coin : Coin.t) -> 
      (Position.dist potionpos coin.pos < 
       (Constant.potion_radius + Constant.coin_radius)) || acc)
      false coins in 
  if occupied then random_valid_tile_potion mz start_pos coins 
  else c, r

(** [random_valid_tile_hourglass mz start_pos coins potions] makes sure 
    the tile a new hourglass is generated on does not already have 
    a coin or potion already on it *)
let rec random_valid_tile_hourglass mz start_pos coins potions = 
  let c, r = random_valid_tile mz in 
  let hourglasspos = (tile_to_pixel start_pos (c, r)) 
                     |> Position.init_pos in 
  let coin_occupied = Array.fold_left (fun acc (coin : Coin.t) -> 
      (Position.dist hourglasspos coin.pos < 
       (Constant.hourglass_radius + Constant.coin_radius)) || acc)
      false coins in 
  let potion_occupied = Array.fold_left (fun acc (potion : Potion.potion) -> 
      (Position.dist hourglasspos potion.pos < 
       (Constant.hourglass_radius + Constant.potion_radius)) || acc)
      false potions in 
  if coin_occupied || potion_occupied 
  then random_valid_tile_hourglass mz start_pos coins potions 
  else c, r

(** [random_valid_tile_enemy mz] is a random valid (non-wall) tile in [mz]
    that is at least 5 tiles away from the origin *)
let rec random_valid_tile_enemy mz = 
  let (x, y) = random_valid_tile mz in 
  if x < 5 || y < 5 then random_valid_tile_enemy mz else (x, y)

let near_enemy (camel : Camel.t) (st : t) = 
  let f (c : Enemy.t) = Position.dist c.pos camel.pos < camel_width in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

let near_genie (camel : Camel.t) (st : t) = 
  match st.genie with 
  | None -> false
  | Some genie -> begin 
      if Position.dist genie.pos camel.pos < camel_width 
      then true else false 
    end 

let shoot (camel : Camel.t) (st : t) = 
  let p = Projectile.init camel.dir camel.pos 
  in {st with projectiles = p :: st.projectiles} 

let move_proj (st : t) = 
  let st' = {st with projectiles = 
                       List.map Projectile.move_proj st.projectiles} in 
  {st' with projectiles = 
              List.filter (fun (p : Projectile.t) -> 
                  not (hit_wall st' p.pos p.dir)) 
                st'.projectiles}

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
                      then acc else x :: acc) 
                    [] accenemy in 
      if List.length remaining = List.length accenemy  
      then check_proj t (h :: accproj, remaining) 
      else check_proj t (accproj, remaining)
  in   
  let (newproj, newenemy) = 
    check_proj st.projectiles ([], Array.to_list st.enemies) in 
  {st with enemies = Array.of_list newenemy; projectiles = newproj}

(** [move_genie genie st] is [genie] with updated position or direction.
    if [genie] will hit a wall then it turns around, otherwise it
    keeps moving in the same direction. *)
let move_genie (st : t) (genie : Genie.genie) : Genie.genie = 
  (* info to compute teleportation *)
  let maze_row = st.rows in
  let maze_col = st.cols in
  let window_height = maze_row * Constant.tile_width + 200 in 
  let window_width = maze_col * Constant.tile_width + 200 in
  let start_y = 
    window_height - ((window_height- maze_row * Constant.tile_width) / 2) in
  let start_x = ((window_width - maze_col * Constant.tile_width) / 2) in
  let start_pos = (start_x, start_y) in
  let genie = if (Unix.gettimeofday ()) -. genie.lastteleport 
                 > Constant.genie_teleport_time then 
      let teleportto = random_valid_tile st.maze 
                       |> Position.tile_to_pixel start_pos 
                       |> Position.init_pos in 
      {genie with pos = teleportto;
                  lastteleport = Unix.gettimeofday ()}
    else genie in 
  (* automate regular movement of genie *)
  let future_pos = (Genie.move genie).pos in 
  if not (hit_wall st future_pos genie.dir) then Genie.move genie
  else 
    let next_move_l = Genie.change_dir genie 180 in
    let next_move_r = Genie.change_dir genie 0 in
    let next_move_up = Genie.change_dir genie 90 in
    let next_move_down = Genie.change_dir genie 270 in 
    let all_moves = [next_move_l; next_move_r; next_move_up; next_move_down] in
    let valid_moves = 
      List.filter (fun (next_move : Genie.genie) -> 
          not (hit_wall st (Genie.move next_move).pos next_move.dir)) 
        all_moves in 
    let random_turn_genie =  
      List.nth valid_moves (Random.int (List.length valid_moves))
    in 
    Genie.move random_turn_genie

(** [update_genie_in_maze st] is [st] with the genie updated, 
    if there is one in it *)
let update_genie_in_maze (st : t) : t = 
  match st.genie with 
  | None -> st
  | Some genie -> begin 
      let genie' = move_genie st genie in 
      {st with genie = Some genie'}
    end 

(** [move_enemy enemy st] is [enemy] with updated position or direction.
    if [enemy] will hit a wall then it turns around, otherwise it
    keeps moving in the same direction. *)
let move_enemy (st : t) (enemy : Enemy.t) : Enemy.t = 
  let future_pos = (Enemy.move enemy).pos in 
  if not (hit_wall st future_pos enemy.dir) then Enemy.move enemy
  else 
    let next_move_l = Enemy.change_dir enemy 180 in
    let next_move_r = Enemy.change_dir enemy 0 in
    let next_move_up = Enemy.change_dir enemy 90 in
    let next_move_down = Enemy.change_dir enemy 270 in 
    let all_moves = [next_move_l; next_move_r; next_move_up; next_move_down] in
    let valid_moves = 
      List.filter (fun (next_move : Enemy.t) -> 
          not (hit_wall st (Enemy.move next_move).pos next_move.dir)) 
        all_moves in 
    let random_turn_enemy =  
      List.nth valid_moves (Random.int (List.length valid_moves))
    in 
    Enemy.move (random_turn_enemy) 

(** [move_enemies st] is the round_state 
    after updating the position of all enemy camels. 
    If the camel has collected the hourglass that freezes enenmies, 
    then it is the original [st] *)
let move_enemies (st : t) : t =
  match st.camel.hourglasses with 
  | None | Some Add -> 
    {st with enemies = Array.map (move_enemy st) st.enemies}
  | Some Pause -> st 

(** [update_camel st] is the round_with the camel's health updated *)
let update_camel (st : t) : t = 
  let camel = st.camel in 
  (* update if near any enemies *)
  let camel = if (near_enemy camel st) 
              && (Unix.gettimeofday ()) -. camel.lasthealthlost 
                 > Constant.health_delay 
    then {camel with health = camel.health - 1; 
                     lasthealthlost = Unix.gettimeofday ()} 
    else camel in 
  (* update if near a genie *)
  let camel, st = if near_genie camel st 
    then {camel with coins = camel.coins + Constant.genie_power}, 
         {st with genie = None}
    else camel, st in 
  (* update if on an hourglass *)
  let camel, st = if on_hourglass camel st 
    then {camel with hourglasses = 
                       match st.hourglass with 
                       | Some hg -> Some hg.power
                       | None -> None},
         {st with hourglass = None}
    else camel, st in 
  {st with camel = camel}  

let remove_coin (c : Coin.t) (st : t) = 
  let coinlst = Array.fold_left 
      (fun acc x -> if x = c then acc else x :: acc) [] st.coins in 
  {st with coins = Array.of_list coinlst}

(** [get_coin st] is [st] with the coin the camel is currently on removed 
    and [camel]'s coin value count updated accordingly *)
let get_coin (st : t) : t = 
  let c = Coin.find_coin st.camel.pos st.coins in 
  let st' = remove_coin c st in 
  {st' with camel = {st'.camel with coins = st'.camel.coins + c.value}} 

let remove_potion (p : Potion.potion) (st : t) = 
  let potlst = Array.fold_left 
      (fun acc x -> if x = p then acc else x :: acc) [] st.potions in 
  {st with potions = Array.of_list potlst}

(** [get_potion st] is [st] with the potion the camel is currently on removed
    and [camel]'s health updated accordingly. 
    Health cannot exceed 3. *)
let get_potion (st : t) : t = 
  let potion = Potion.find_potion st.camel.pos st.potions in 
  let st' = remove_potion potion st in 
  let health' = if st.camel.health = 3 then 3 else st.camel.health + 1 in 
  {st' with camel = {st'.camel with health = health'}}

let update_round_state (st : t) : t = 
  let st' = update_camel st |> move_proj 
            |> move_enemies |> hit_enemy
            |> update_genie_in_maze in 
  let st'' = if on_coin st' then get_coin st' else st' in 
  if on_potion st'' then get_potion st'' else st''

(************************************************************
   initialization
 ***********************************************************)
(** [tile_close_to_start (x,y)] is if the tile [(x,y)] is 
    within 8 tiles of the start tile (0,0) *)
let tile_close_to_start (x,y) = 
  if x < 9 || y < 9 then false else true 

(** [init_enemy_lst n mz] is an Array of [n] enemy camels 
    with valid positions in [mz] *)
let init_enemy_lst (n : int) (mz : Maze.maze) (start_pos): Enemy.t array = 
  Array.init n (fun i -> 
      (Enemy.init (90 * Random.int 4) 
         (random_valid_tile mz 
          |> Position.tile_to_pixel start_pos 
          |> Position.init_pos)))

(** [init_coin_lst n mz] is an Array of [n] coins 
    with valid positions in [mz]. *)
let init_coin_lst n mz start_pos=
  Array.init n (fun i -> 
      (Coin.init 
         (random_valid_tile mz 
          |> Position.tile_to_pixel start_pos 
          |> Position.init_pos) (10 * Random.int 4 + 10)))

(** [init_potion_lst n mz start_pos coins] is an Array of [n] potions 
    with valid positions in [mz] (one that is not a wall 
    or already occupied by a coin). *)
let init_potion_lst n mz start_pos coins =
  Array.init n (fun i -> 
      (Potion.init 
         (random_valid_tile_potion mz start_pos coins
          |> Position.tile_to_pixel start_pos 
          |> Position.init_pos)))

(** [init_genie mz start_pos] is a genie with a valid position in [mz] *)
let init_genie mz start_pos = 
  Some (Genie.init
          (90 * Random.int 4) 
          (random_valid_tile mz 
           |> Position.tile_to_pixel start_pos 
           |> Position.init_pos))

(** [init_hourglass mz start_pos coins potions] is an hourglass with
    a valid position in [mz], one that is not a tile
    or already occupied by a coin or a potion *)
let init_hourglass mz start_pos coins potions = 
  Hourglass.init
    (random_valid_tile_hourglass mz start_pos coins potions 
     |> Position.tile_to_pixel start_pos 
     |> Position.init_pos)    

let init cols rows numenemy difficulty = 
  let mz = Maze.populate cols rows (0,0) in 
  let maze_row = rows in
  let maze_col = cols in
  let window_height = maze_row * Constant.tile_width + 200 in 
  let window_width = maze_col * Constant.tile_width + 200 in
  let start_y = 
    window_height - ((window_height- maze_row * Constant.tile_width) / 2) in
  let start_x = ((window_width - maze_col * Constant.tile_width) / 2) in
  let start_pos = (start_x, start_y) in
  let camel = Camel.init ((fst start_pos) + camel_radius) 
      ((snd start_pos) - camel_radius) in
  let numpotions = if numenemy = 0 then 0 else 2 in 
  let genie = if numenemy = 10 then init_genie mz start_pos 
    else None in 
  let coinarr = init_coin_lst 20 mz start_pos in 
  let potionarr = init_potion_lst numpotions mz start_pos coinarr in 
  let hourglass = if difficulty = 1 then None else 
    if numenemy <> 0 then Some (init_hourglass mz start_pos coinarr potionarr)
    else None in 
  Graphics.resize_window window_width window_height;
  {camel = camel; 
   maze = mz;
   cols = cols;
   rows = rows;
   enemies = init_enemy_lst numenemy mz start_pos;
   coins = coinarr;
   potions = potionarr;
   projectiles = [];
   genie = genie;
   hourglass = hourglass;
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
  "\n" ^ "Potions: " ^ pp_array st.potions Potion.string_of_potion ^ 
  "\n" ^  "Projectiles: " ^ pp_lst st.projectiles Projectile.string_of_proj