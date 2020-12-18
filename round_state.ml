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
  top_left_corner : int * int;
  portals : Position.t list
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

let find_four_corners (pos : Position.t) (dir : int) = 
  let tl = (pos.x - Constant.camel_radius, pos.y + Constant.camel_radius) in
  let tr = (pos.x + Constant.camel_radius, pos.y + Constant.camel_radius) in
  let bl = (pos.x - Constant.camel_radius, pos.y - Constant.camel_radius) in
  let br = (pos.x + Constant.camel_radius, pos.y - Constant.camel_radius) in
  (tl, tr, br, bl)

let find_head (pos : Position.t) (dir : int) = 
  let (tl, tr, br, bl) = find_four_corners pos dir in
  match dir with 
  | 0 -> (tr, br)
  | 90 -> (tl, tr)
  | 180 -> (tl, bl)
  | 270 -> (bl, br)
  | _ -> failwith "impossible"

let find_tail (pos : Position.t) (dir : int) = 
  let (tl, tr, br, bl) = find_four_corners pos dir in
  match dir with 
  | 0 -> (tl, bl)
  | 90 -> (bl, br)
  | 180 -> (tr, br)
  | 270 -> (tl, tr)
  | _ -> failwith "impossible"

let hit_wall (st : t) (pos : Position.t) (dir : int) = 
  let two_corners = find_head pos dir in
  Graphics.moveto (fst st.top_left_corner + 3) (snd st.top_left_corner + 30);
  Graphics.set_color Graphics.black; 
  Graphics.fill_rect (fst st.top_left_corner + 3) (snd st.top_left_corner + 30) 33 20;  
  Graphics.set_color Graphics.red;
  Graphics.draw_string (if (hit_corner st (Position.init_pos (fst two_corners)) || hit_corner st 
                              (Position.init_pos (snd two_corners)))then 
                          "wall" else "path");                          
  hit_corner st (Position.init_pos (fst two_corners)) || hit_corner st 
    (Position.init_pos (snd two_corners))

let backwards_dir = function
  | 90 -> 270
  | 180 -> 0
  | 270 -> 90
  | 0 -> 180
  | _ -> failwith "impossible"

let move_camel_ice (st : t) (camel : Camel.t) : Camel.t =  
  let og_dir = camel.dir in 
  let back_dir = backwards_dir og_dir in
  let next_move_l = Camel.change_dir camel 180 in
  let next_move_r = Camel.change_dir camel 0 in
  let next_move_up = Camel.change_dir camel 90 in
  let next_move_down = Camel.change_dir camel 270 in 
  let all_moves = [next_move_l; next_move_r; next_move_up; next_move_down] in
  let valid_moves = List.filter (fun next_move -> 
      not (hit_wall st (Camel.move next_move).pos next_move.dir)) all_moves in 
  Graphics.moveto (fst st.top_left_corner + 3) (snd st.top_left_corner + 60);
  Graphics.set_color Graphics.black; 
  Graphics.fill_rect (fst st.top_left_corner + 3) (snd st.top_left_corner + 60) 600 20;  
  Graphics.set_color Graphics.red;
  Graphics.draw_string (" number of valid moves: " ^ 
                        (string_of_int (List.length valid_moves)));
  Graphics.draw_string ("  og dir " ^ 
                        (string_of_int (og_dir)));
  let final_valid_moves = if List.length valid_moves > 1 then 
      List.filter (fun (next : Camel.t) -> next.dir <> back_dir) valid_moves
    else valid_moves
  in 
  Graphics.draw_string ("  number of valid moves that are not last move: " ^ 
                        (string_of_int (List.length final_valid_moves)));
  let random_turn_camel =  List.nth final_valid_moves 
      (Random.int (List.length final_valid_moves)) in
  Camel.move (random_turn_camel) 

let restore_speed_camel camel = 
  if camel.speed <> Constant.camel_mud_speed then camel 
  else {camel with speed = Constant.camel_speed} 

let restore_shoot_camel camel = 
  if camel.shoot then camel else {camel with shoot = true} 

let restore_teleport_camel camel = 
  if not camel.teleport then camel else {camel with teleport = false} 

let restore_default_camel (camel : Camel.t) = 
  let speedy_camel = restore_speed_camel camel in 
  let shooty_camel = restore_shoot_camel speedy_camel in
  restore_teleport_camel shooty_camel

let handle_hit_ice (st : t) (camel : Camel.t) = 
  if camel.shoot then let new_camel = 
                        restore_teleport_camel (restore_speed_camel camel) in
    move_camel_ice st {new_camel with shoot = false; last_tile = Power_Path Ice} 
  else move_camel_ice st camel 

let handle_hit_mud (camel : Camel.t) = 
  if camel.last_tile = Maze.Power_Path Mud then camel 
  else let speedy_camel = restore_teleport_camel (restore_shoot_camel camel) in 
    {speedy_camel with speed = Constant.camel_mud_speed; last_tile = Power_Path
                                                             Mud}

let in_tile (st : t) (camel : Camel.t) = function
  | Valid (col, row) -> 
    let (cx, cy) = Position.tile_to_pixel (st.top_left_corner) (col, row) in 
    Graphics.draw_string ("abs " ^ (string_of_int (Int.abs (camel.pos.x - cx) + Int.abs (camel.pos.y - cy))));
    (Int.abs (camel.pos.x - cx) + Int.abs (camel.pos.y - cy)) < 20
  | Out_of_bounds -> false

let handle_hit_portal (st : t) (camel : Camel.t) = 
  let curr_tile = Position.pixel_to_tile (Position.init_pos (fst (find_head camel.pos camel.dir))) (st.top_left_corner) in
  Graphics.moveto (100) (60);
  Graphics.set_color Graphics.black; 
  Graphics.fill_rect (100) (60) 250 20; 
  Graphics.set_color Graphics.white;
  Graphics.draw_string (" in tile " ^ (string_of_bool (in_tile st camel curr_tile)));
  Graphics.draw_string (" teleport " ^ (string_of_bool (camel.teleport))); 
  if camel.last_tile <> Maze.Power_Path Portal then 
    let default_camel = restore_default_camel camel in 
    {default_camel with last_tile = Power_Path Portal} 
  else 
    let curr_tile = Position.pixel_to_tile 
        (Position.init_pos (fst (find_head camel.pos camel.dir))) 
        (st.top_left_corner) 
    in
    if not (in_tile st camel curr_tile) || camel.teleport then camel 
    else 
      let possible_portals = List.filter (fun ppos -> 
          (Position.pixel_to_tile ppos st.top_left_corner) <> curr_tile) st.portals 
      in
      Graphics.moveto (100) (60);
      Graphics.set_color Graphics.black; 
      Graphics.fill_rect (100) (60) 250 20; 
      Graphics.set_color Graphics.white;
      Graphics.draw_string (string_of_int (List.length st.portals));
      Graphics.draw_string (" " ^ (string_of_int (List.length possible_portals))); 
      let new_pos = List.nth possible_portals 
          (Random.int (List.length possible_portals)) in  
      Graphics.set_color Graphics.white;
      Graphics.draw_string (" " ^ (string_of_int new_pos.x)); 
      Graphics.draw_string (" " ^ (string_of_int new_pos.y)); 
      Camel.teleport {camel with teleport = true} new_pos

let hit_power_tile (st : t) (pos : Position.t) =
  Graphics.moveto (fst st.top_left_corner - 50) (snd st.top_left_corner + 30);
  Graphics.set_color Graphics.black; 
  Graphics.fill_rect (fst st.top_left_corner - 50) (snd st.top_left_corner + 30) 250 20;  
  let (head, _) = find_head pos st.camel.dir in 
  let (tail, _) = find_tail pos st.camel.dir in
  let head_tile = Position.pixel_to_tile (Position.init_pos head) 
      st.top_left_corner in
  let tail_tile = Position.pixel_to_tile (Position.init_pos tail) 
      st.top_left_corner in
  match head_tile, tail_tile with 
  | Valid (ch, rh), Valid (ct, rt) -> begin
      match (Maze.tile_type st.maze ch rh), (Maze.tile_type st.maze ct rt) with 
      | Power_Path Ice, _ -> 
        Graphics.moveto (fst st.top_left_corner + 50) (snd st.top_left_corner + 30);
        Graphics.set_color Graphics.black; 
        Graphics.fill_rect (fst st.top_left_corner + 50) (snd st.top_left_corner + 30) 33 20;  
        Graphics.set_color Graphics.white;
        Graphics.draw_string "Ice!"; 
        handle_hit_ice st st.camel 
      | Power_Path Mud, _ | _, Power_Path Mud -> 
        Graphics.moveto (fst st.top_left_corner + 100) (snd st.top_left_corner + 30);
        Graphics.set_color Graphics.black; 
        Graphics.fill_rect (fst st.top_left_corner + 100) (snd st.top_left_corner + 30) 33 20;  
        Graphics.set_color Graphics.white;
        Graphics.draw_string "Mud!";  
        handle_hit_mud st.camel
      | Power_Path Portal, _ | _ , Power_Path Portal -> 
        Graphics.moveto (fst st.top_left_corner + 150) (snd st.top_left_corner + 30);
        Graphics.set_color Graphics.black; 
        Graphics.fill_rect (fst st.top_left_corner + 150) (snd st.top_left_corner + 30) 33 20;  
        Graphics.set_color Graphics.white;
        Graphics.draw_string "Portal!";  
        handle_hit_portal st st.camel
      | _ -> 
        Graphics.moveto (fst st.top_left_corner - 50) (snd st.top_left_corner + 30);
        Graphics.set_color Graphics.black; 
        Graphics.fill_rect (fst st.top_left_corner - 50) (snd st.top_left_corner + 30) 33 20;  
        Graphics.set_color Graphics.white;
        Graphics.draw_string "None!"; 
        {st.camel with speed = Constant.camel_speed; shoot = true; 
                       last_tile = Maze.Path; teleport = false}
    end
  | _ -> failwith "impossible"


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

(** [random_valid_tile_enemy mz] is a random valid (non-wall) tile in [mz]
    that is at least 5 tiles away from the origin *)
let rec random_valid_tile_enemy mz = 
  let (x, y) = random_valid_tile mz in 
  if x < 5 || y < 5 then random_valid_tile_enemy mz else (x, y)

(** [random_valid_tile_enemy mz] is a random valid (non-wall and
    non-portal) tile in [mz] *)
let rec random_valid_tile_portal mz = 
  let (col, row) = (2,0)in 
  if not (Maze.tile_type mz col row = Maze.Path) then 
    (1,1) 
  else (col, row) 
(* let (col, row) = random_valid_tile mz in 
   if not (Maze.tile_type mz col row = Maze.Path) then 
   random_valid_tile_portal mz 
   else (col, row)  *)


let near_enemy (camel : Camel.t) (st : t) = 
  let f (c : Enemy.t) = Position.dist c.pos camel.pos < camel_width in 
  Array.fold_left (fun acc x -> (f x) || acc) false st.enemies

let shoot (camel : Camel.t) (st : t) = 
  if camel.shoot then 
    let p = Projectile.init camel.dir camel.pos 
    in {st with projectiles = p :: st.projectiles} 
  else st

let move_proj (st : t) = 
  let st' = {st with projectiles = 
                       List.map Projectile.move_proj st.projectiles} in 
  {st' with projectiles = 
              List.filter (fun (p : Projectile.t) -> 
                  not (hit_wall st' p.pos p.dir)) 
                st'.projectiles}

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
    let valid_moves = List.filter (fun next_move -> 
        not (hit_wall st (move next_move).pos next_move.dir)) all_moves in 
    let random_turn_enemy =  
      List.nth valid_moves (Random.int (List.length valid_moves))
    in 
    move (random_turn_enemy) 

let move_enemies (st : t) : t =
  {st with enemies = Array.map (move_enemy st) st.enemies}

(* [update_camel st] is the round_with the camel's 
   health and coin total updated *)
let update_camel (st : t) : t = 
  let camel = st.camel in 
  let camel' = if (near_enemy camel st) 
               && (Unix.gettimeofday ()) -. camel.lasthealthlost 
                  > Constant.health_delay 
    then {camel with health = camel.health - 1; 
                     lasthealthlost = Unix.gettimeofday ()} 
    else camel in 
  {st with camel = camel'}  

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

let update_round_state (st : t) : t = 
  let st' = update_camel st |> move_proj |> move_enemies |> hit_enemy in 
  if on_coin st' then get_coin st' else st' 

(************************************************************
   initialization
 ***********************************************************)
(** [init_portals n mz acc] is an Array of [n] portals with valid positions, 
    followed by a list of positions where the portals are located *)
let rec init_portals (start_pos : int * int)
    (nportals : int) (mz : Maze.maze) (acc : (Position.t list)) 
  : (Maze.maze * Position.t list) = 
  let (col, row) = random_valid_tile_portal mz in 
  mz.(row).(col) <- Power_Path Portal; 
  let (x, y) = Position.tile_to_pixel start_pos (col, row) in
  if nportals > 1 then begin
    init_portals start_pos (nportals - 1) mz ((Position.init_pos (x, y)) :: acc)
  end
  else (mz, ((Position.init_pos (x, y)) :: acc))

(** [init_enemy_lst n mz] is an Array of [n] enemy camels with valid positions *)
let init_enemy_lst (n : int) (mz : Maze.maze) (start_pos): Enemy.t array = 
  Array.init n (fun i -> 
      (Enemy.init (90 * Random.int 4) 
         (random_valid_tile mz 
          |> Position.tile_to_pixel start_pos 
          |> Position.init_pos)))

(** [init_coin_lst n mz] is an Array of [n] coins with valid positions in [mz]. *)
let init_coin_lst n mz start_pos=
  Array.init n (fun i -> 
      (Coin.init 
         (random_valid_tile mz 
          |> Position.tile_to_pixel start_pos 
          |> Position.init_pos) (10 * Random.int 4 + 10)))

let init cols rows numenemy portals = 
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
  let (mz_w_portals, portal_lst) = init_portals start_pos portals mz [] in
  Graphics.resize_window window_width window_height;
  {camel = camel; 
   maze = mz_w_portals;
   cols = cols;
   rows = rows;
   enemies = init_enemy_lst numenemy mz start_pos;
   coins = init_coin_lst 20 mz start_pos;
   projectiles = [];
   top_left_corner = start_pos;
   portals = portal_lst}

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