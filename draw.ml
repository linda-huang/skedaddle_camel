open Graphics
open Camel
open Enemy
open Maze 
open Round_state
open Game_state 
open Scorer 
open Coin
open Constant 
open Timer 
open Background
open Img_enemy_camel
open Img_heart
open Img_tile
open Words
open Instructions

let prev_health = ref num_lives

let rec draw_words height (pos : Position.t) = function
  | [] -> ()
  | h :: t -> 
    Graphics.moveto pos.x pos.y;
    Graphics.draw_string h;
    draw_words height (Position.init_pos (pos.x, pos.y - height)) t

(** [initialize_lives]  draws [num_lives] number of hearts at (x,y) as top
    left corner of the first heart. *)
let initialize_lives (x, y) health = 
  let counter = ref 0 in
  let full_heart = Graphics.make_image Img_heart.full_heart in
  let empty_heart = Graphics.make_image Img_heart.empty_heart in 
  while !counter < health do 
    Graphics.draw_image full_heart 
      (x + heart_size / 2 + 
       !counter * (Constant.heart_size + (Constant.heart_size / 4)))
      (y - Constant.heart_size);
    incr counter 
  done;
  while !counter < Constant.num_lives do
    Graphics.draw_image empty_heart
      (x + heart_size / 2 + 
       !counter * (Constant.heart_size + (Constant.heart_size / 4))) 
      (y - Constant.heart_size);
    incr counter;
  done

(** [reduce_hearts_img (x,y) loss num_lives] draws black heart for every life
    that is lost.
    [(x,y)] is the top left corner of the maze
    [loss] is the number of hearts lost
    [num_lives] is the current number of lives camel has.*)
let reduce_hearts_img (x, y) num_lives = 
  let empty_heart = Graphics.make_image Img_heart.empty_heart in 
  Graphics.draw_image empty_heart 
    (x + heart_size / 2 +
     num_lives * (Constant.heart_size + (Constant.heart_size / 4)))
    (y + 10)

(** [add_hearts_img (x,y) num_lives] draws red heart for every life
    that is gained.
    [(x,y)] is the top left corner of the maze
    [num_lives] is the current number of lives camel has.*)
let add_heart_img (x, y) num_lives = 
  let full_heart = Graphics.make_image Img_heart.full_heart in 
  Graphics.draw_image full_heart 
    (x + heart_size / 2 + (num_lives - 1) *
                          (Constant.heart_size + (Constant.heart_size / 4)))
    (y + 10)

(** [update_hearts (x,y) prev_h curr_health] updates the amount of 
    hearts drawn appropriately *)
let update_hearts (x, y) prev_h curr_health = 
  if curr_health < prev_h then 
    reduce_hearts_img (x, y) curr_health
  else if curr_health > prev_h then 
    add_heart_img (x, y) curr_health
  else ();
  prev_health := curr_health

(** [draw_background ()] draws the background over the entire window *)
let draw_background ()= 
  let image = Graphics.make_image Background.background in 
  Graphics.draw_image image 0 0

(** [update_coin_value st coin_val] draws the coin value appropriately *)
let update_coin_value st coin_val = 
  let x,y = st.top_left_corner in 
  let posx, posy = (x + tile_width + heart_size / 2,
                    y - (st.rows + 1) * tile_width + heart_size / 4) in 
  set_color 0x026144;
  fill_rect posx posy heart_size heart_size;
  set_color 0xffe524;
  moveto posx posy;
  let coin_string = string_of_int coin_val in
  let coin_str = 
    match String.length coin_string with 
    | 1 -> "000" ^ coin_string
    | 2 -> "00" ^ coin_string
    | 3 -> "0" ^ coin_string
    | 4 -> coin_string
    | _ -> coin_string
  in
  draw_string coin_str

(** [update_time_left st time_left] draws the [time_left] in [st] 
    nicely on the screen *)
let update_time_left st time_left= 
  let x, y = st.top_left_corner in 
  let posx, posy = (x + tile_width * 4 - heart_size / 4, 
                    y - (st.rows + 1) * tile_width + heart_size / 4) in 
  moveto posx posy;
  set_color 0x026144;
  fill_rect posx posy (tile_width * 5) heart_size;
  set_color 0xffe524;
  if time_left >= 0 then 
    let sec = time_left mod 60 in
    let min = time_left / 60 in 
    draw_string  ((string_of_int min) ^ ":" ^ (string_of_int sec))
  else 
    draw_string "infinity"

(** [update_time_lapsed st time] draws the elapsed time *)
let update_time_lapsed st time = 
  let x, y = st.top_left_corner in 
  let posx, posy = (x + (st.cols / 2 * tile_width) + heart_size * 2, 
                    y + heart_size/2) in
  let hour = time / 3600 in 
  let hour_str = if hour > 9 then string_of_int hour 
    else "0" ^ string_of_int hour in 
  let min = time / 60 in 
  let min_str = if min > 9 then string_of_int min
    else "0" ^ string_of_int min in 
  let sec = time mod 60 in 
  let sec_str = if sec > 9 then string_of_int sec 
    else "0" ^ string_of_int sec in
  set_color 0x026144;
  fill_rect posx posy (tile_width * 3) heart_size;
  set_color 0xffe524;
  moveto posx posy;
  draw_string (hour_str ^ ":" ^ min_str ^ ":" ^ sec_str)

let draw_initial_round_state st coin_val =
  draw_background ();
  let x, y = st.top_left_corner in 
  let posx, posy = (x, y - (st.rows + 1) * tile_width) in
  let coin_img = make_image coin_symbol in 
  set_color 0x026144;
  fill_rect (x - 1) (y + 1) 
    (tile_width * st.cols) (tile_width + heart_size / 3);
  fill_rect (posx - 1) (posy - heart_size / 3) 
    (tile_width * st.cols) (tile_width + heart_size/ 3);
  draw_image coin_img (posx + heart_size / 2) posy;
  update_coin_value st coin_val;
  draw_image (make_image hourglass) 
    (posx + tile_width * 3) (posy);
  update_time_left st (-1);
  initialize_lives (x, y + heart_size + 10) st.camel.health;
  moveto (x + (st.cols / 2 * tile_width) - 2 * tile_width) (y + heart_size / 2);
  set_color 0xffe524;
  draw_string "TIME ELAPSED: ";
  update_time_lapsed st 0

let draw_element x y color size= 
  set_color color;
  fill_poly [|(x-1,y+1); (x+size,y+1); (x+size, y-size);(x-1, y-size)|]

let draw_walls (gen_maze : Maze.maze) start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i * Constant.tile_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + j * Constant.tile_width, snd !curr_pos);
      let tile = tile_type gen_maze j i in
      match tile with 
      | Path -> let path_img = make_image sand_tile2 in 
        draw_image path_img (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | (Wall hp) when hp > 0 -> let wall_img = make_image sand_wall in 
        draw_image wall_img (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | Start -> let start_tile = make_image portal_tile in 
        draw_image start_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | Exit -> let end_tile = make_image portal_tile2 in 
        draw_image end_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | Power_Path Ice -> let ice_tile = make_image ice_path in
        draw_image ice_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | Power_Path Mud -> let mud_tile = make_image mud_path in 
        draw_image mud_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | Power_Path Portal -> let portal_tile = make_image portal_path in 
        draw_image portal_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      | _ -> failwith "impossible"
    end
    done
  end
  done

let draw_maze (st : Round_state.t) = 
  let start_pos = (fst st.top_left_corner, snd st.top_left_corner) in
  moveto (fst start_pos) (snd start_pos);
  Graphics.set_text_size 300; 
  Graphics.set_color Graphics.black;
  draw_walls st.maze start_pos st.rows st.cols

let draw_camel (camel : Camel.t) = 
  let (x, y) = (camel.pos.x, camel.pos.y) in 
  let camel_pic = match camel.dir with 
    | 0 -> make_image camel_0
    | 90 -> make_image camel_90
    | 180 -> make_image camel_180
    | 270 -> make_image camel_270
    | _ -> make_image camel_0
  in 
  draw_image camel_pic (x - camel_radius) (y - camel_radius)

let draw_enemy (enemy : Enemy.t) = 
  let enemy_pic = 
    match enemy.dir with 
    | 0 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_0
    | 90 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_90
    | 180 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_180
    | 270 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_270
    | _ -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_0
  in
  let (x, y) = (enemy.pos.x, enemy.pos.y) in 
  Graphics.draw_image enemy_pic 
    (x - Constant.camel_radius) (y - Constant.camel_radius)

let draw_coin (coin : Coin.t) =
  let (x, y) = (coin.pos.x, coin.pos.y) in 
  let coin_img = make_image coin_pic in 
  draw_image coin_img (x - coin_radius) (y - coin_radius)

(** [draw_potion potion] draws a potion icon
    by the position defined by [potion] *)
let draw_potion (potion : Potion.potion) = 
  let (x, y) = (potion.pos.x - 10, potion.pos.y - 10) in 
  let small_heart = make_image heart_small in 
  draw_image small_heart x y

(** [draw_projectile proj] draws a projectile icon
    by the position defined by [proj] *)
let draw_projectile (proj : Projectile.t) =
  Graphics.set_color Constant.projectile_color;
  let (x, y) = (proj.pos.x, proj.pos.y) in 
  fill_poly [|(x-projectile_radius, y+projectile_radius); 
              (x+projectile_radius, y+projectile_radius); 
              (x+projectile_radius, y-projectile_radius); 
              (x-projectile_radius, y-projectile_radius)|]

(** [draw_genie genie] draws a genie icon 
    at the position defined by [genie] *)
let draw_genie (genie : Genie.genie option) = 
  match genie with 
  | None -> ()
  | Some genie -> begin 
      let x, y = (genie.pos.x - 15, genie.pos.y - 15) in 
      let genie = make_image genie_pic in 
      draw_image genie x y
    end 

(** [draw_hourglass hourglass] draws a hourglass icon
    at the position defined by [hourglass] and differentiates the 
    image based on [hourglass]'s power *)
let draw_hourglass (hourglass : Hourglass.hourglass option) = 
  match hourglass with 
  | None -> ()
  | Some hourglass -> begin
      let x, y = (hourglass.pos.x - 10, hourglass.pos.y - 10) in 
      match hourglass.power with 
      | Add -> let hourglass_img = make_image hourglass_small in 
        draw_image hourglass_img x y
      | Pause -> let magic_wand = make_image wand_pic in 
        draw_image magic_wand x y
    end

(** [draw_hourglass_msg x y] is the message in the transition screen 
    printed at [x,y] to explain the hourglasses *)
let draw_hourglass_msg x y  = 
  let y = y - 10  in Graphics.moveto (x - 5) y;
  Graphics.set_color Graphics.black; 
  draw_words 15 (Position.init_pos (x, y)) hourglass_txt

let draw_round_state (st : Round_state.t) = 
  draw_maze st;
  draw_camel st.camel; 
  Array.iter draw_enemy st.enemies; 
  List.iter draw_projectile st.projectiles;
  Array.iter draw_coin st.coins;
  Array.iter draw_potion st.potions;
  draw_genie st.genie;
  draw_hourglass st.hourglass;
  Graphics.synchronize ()

let draw_prewelcome () = 
  Graphics.set_window_title "Skedaddle Camel";
  draw_background ();
  Graphics.set_text_size 300;

  let title = make_image title_pic_transp in 
  draw_image title 35 50;
  Graphics.moveto 350 100;
  Graphics.synchronize ()

let draw_welcome () = 
  Graphics.clear_graph ();
  draw_background ();
  Graphics.set_window_title "Skedaddle Camel";
  Graphics.set_text_size 300;
  let x, y = 20, 650 in 
  Graphics.moveto x y;
  draw_words 30 (Position.init_pos (x,y)) welcome_txt;
  Graphics.set_color Graphics.black;
  Graphics.synchronize ()

let helper_stats x y difficulty scr coins = 
  Graphics.moveto x y;
  Graphics.draw_string ("Game difficulty: " ^ difficulty);
  Graphics.moveto x (y - 25);
  Graphics.draw_string ("Enemies killed: " ^ string_of_int scr.hit);
  Graphics.moveto x (y - 50);
  Graphics.draw_string ("Coins collected: " ^ string_of_int coins);
  Graphics.moveto x (y - 75)

(** [draw_finscore gs] draws the score corresponding to [gs] *)
let draw_finscore (gs : Game_state.game_state) = 
  Graphics.set_color Graphics.black; 
  let st = gs.round_state in 
  let scr = gs.score in 
  let coins = scr.coins in 
  let difficulty = 
    match gs.game_difficulty with 
    | Easy -> "Easy"
    | Hard -> "Hard"
  in 
  let x, y = (fst st.top_left_corner, snd st.top_left_corner - 25) in
  helper_stats x y difficulty scr coins;
  let calculated_score = 
    match gs.game_difficulty with 
    | Easy -> Scorer.score scr st.camel false
    | Hard -> Scorer.score scr st.camel true 
  in 
  Graphics.set_color Graphics.red;
  Graphics.draw_string ("Final score: " ^ string_of_int calculated_score);
  Graphics.synchronize ()

let helper_draw_transition_welcome x y t = 
  Graphics.moveto x y;
  Graphics.set_text_size 300; 
  Graphics.set_color Graphics.blue;
  Graphics.draw_string ("Welcome to level " ^ string_of_int (t + 1));
  let y = y - 25 in Graphics.moveto x y;
  Graphics.draw_string "(Press `x` to start the next level)";
  Graphics.set_color Graphics.black

let helper_game_difficulty x y = function
  | 0 -> Graphics.draw_string "There is no time limit"
  | 1 -> Graphics.draw_string "You have 100 seconds to escape this level!";
    draw_hourglass_msg x y
  | 2 -> Graphics.draw_string "You have 60 seconds to escape this level!";
    draw_hourglass_msg x y
  | _ -> ()

let helper_num_enemies x y = function
  | 0 -> Graphics.draw_string "This level has 0 enemies"
  | 1 -> Graphics.draw_string "This level has 2 enemies";
    let y = y - 15 in Graphics.moveto x (y - 15);
    Graphics.draw_string "There are two hearts you can collect to gain more health";
  | 2 -> Graphics.draw_string "This level has 8 enemies";
    draw_words 30 (Position.init_pos (x, (y - 15))) genie_txt
  | _ -> ()

let helper_calc_score gs st = function
  | Easy -> Scorer.score gs.score st.camel false
  | Hard -> Scorer.score gs.score st.camel true 

let draw_transition (t : int) (gs : Game_state.game_state) : unit = 
  draw_background ();
  let st = gs.round_state in 
  let x, y = (fst st.top_left_corner, snd st.top_left_corner - 10) in
  helper_draw_transition_welcome x y t;
  let calculated_score = if t = 0 then 0 else 
      helper_calc_score gs st gs.game_difficulty
  in 
  let y = y - 50 in Graphics.moveto (x + 25) y;
  Graphics.draw_string ("Score so far: " ^ string_of_int calculated_score);
  let y = y - 15 in Graphics.moveto x y;
  let _ = helper_num_enemies x (y - 100) t in 
  Graphics.set_color Graphics.black; 
  let _ = match gs.game_difficulty with 
    | Easy -> ();
    | Hard -> begin
        let y = y - 35 in Graphics.moveto x y;
        helper_game_difficulty x y t
      end 
  in Graphics.synchronize () 

let helper_draw_instructions x y = 
  Graphics.moveto x y; Graphics.set_text_size 300; 
  Graphics.set_color Graphics.blue; Graphics.draw_string ("INSTRUCTIONS. ");
  Graphics.set_color Graphics.red; 
  Graphics.moveto x (y-25);
  Graphics.draw_string "(Press `x` to return to the game)";
  Graphics.set_color Graphics.black

(** [draw_instructions gs] draws the instructions of the game
    during game play, if requested *)
let draw_instructions (gs : Game_state.game_state) timer i : unit = 
  Graphics.clear_graph ();
  let st = gs.round_state in 
  let x, y = (fst st.top_left_corner, snd st.top_left_corner) in
  helper_draw_instructions x y;
  draw_words 30 (Position.init_pos (x, y-40)) instructions_txt;
  let _ = match gs.game_difficulty with 
    | Easy -> ();
    | Hard -> begin
        let y = y - 400 in Graphics.moveto x y;
        Graphics.set_color Graphics.black; 
        let y = y - 15 in Graphics.moveto x y;
        Graphics.draw_string "There are two types of hourglasses: usually, it only adds time";
        let y = y - 15 in Graphics.moveto (x + 25) y;
        Graphics.draw_string "There is a rare hourglass that pauses enemy movement";
      end
  in 
  Graphics.synchronize () 

let draw_gameover (gs : Game_state.game_state) (over : Game_state.game_end) = 
  draw_background ();
  let msg = match over with 
    | Time -> "You ran out of time!"
    | Health -> "You ran out of lives!"
  in 
  let x, y = (fst gs.round_state.top_left_corner, 
              snd gs.round_state.top_left_corner) in
  Graphics.moveto x y;
  Graphics.draw_string ("Game over! " ^ msg);
  Graphics.set_color Graphics.black;
  draw_finscore gs; 
  Graphics.synchronize ();
  let rec exit_game () = 
    match Graphics.read_key () with 
    |'0' -> exit 0
    | _ -> exit_game () 
  in exit_game () 

let draw_won (gs : Game_state.game_state) : unit = 
  draw_background ();
  let x, y = (fst gs.round_state.top_left_corner, 
              snd gs.round_state.top_left_corner) in
  Graphics.moveto x y;
  Graphics.draw_string "Congratulations! You've escaped!";
  draw_finscore gs; 
  Graphics.synchronize ();
  let rec exit_game () = 
    match Graphics.read_key () with 
    |'0' -> exit 0
    | _ -> exit_game () 
  in exit_game () 

(** [draw_time gs timer] draws the time elapsed during a round
    and time remaining, if the difficulty of [gs] is set to Hard *)
let draw_time (gs : Game_state.game_state) (timer : Timer.timer) = 
  let st = gs.round_state in 
  update_time_lapsed st timer.elapsedtime;
  match gs.game_difficulty with 
  | Easy -> ()
  | Hard -> begin 
      let curr_round = 
        if gs.score.mazes = 0 then Constant.round1 
        else if gs.score.mazes = 1 then Constant.round2 
        else Constant.round3 in 
      match Timer.time_left curr_round gs.round_state timer with 
      | None -> () 
      | Some time -> begin 
          update_time_left gs.round_state time;
        end 
    end

(** [draw_level_num gs] draws the number level the player is currently on *)
let draw_level_num gs = 
  let st = gs.round_state in 
  let start_pos = (fst st.top_left_corner, snd st.top_left_corner + 10) in
  Graphics.moveto (fst start_pos) (snd start_pos + 50);
  Graphics.set_text_size 300; 
  Graphics.set_color Graphics.black;
  if gs.score.mazes = 0 then Graphics.draw_string "ROUND 1"
  else if gs.score.mazes = 1 then Graphics.draw_string "ROUND 2"
  else Graphics.draw_string "ROUND 3"

let draw_game_state (gs : Game_state.game_state) (timer : Timer.timer) = 
  let st = gs.round_state in 
  match gs.current_state with 
  | PreWelcome -> draw_prewelcome ()
  | Welcome -> draw_welcome ()
  | InPlay -> draw_round_state gs.round_state; 
    draw_level_num gs; 
    let x = fst st.top_left_corner in 
    Graphics.moveto (x + 10) 10;
    Graphics.set_text_size 50;
    Graphics.set_color 0xB03F37;
    update_coin_value st (gs.score.coins + gs.round_state.camel.coins);
    Graphics.set_color 0xFCF25D;
    update_hearts st.top_left_corner !prev_health st.camel.health;
    draw_time gs timer
  | Transition t -> draw_transition t gs 
  | Instructions i -> draw_instructions gs timer i 
  | Won -> draw_won gs
  | GameOver over -> draw_gameover gs over



