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

(* set_font "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1"; *)
(* set_font "-adobe-courier-medium-r-normal--0-0-0-0-m-0-iso8859-1"; *)
(* set_font "-adobe-helvetica-bold-r-normal--0-0-0-0-p-0-iso8859-1"; *)

(* [initialize_lives]  draws [num_lives] number of hearts at (x,y) as top
   left corner of the first heart. *)
let initialize_lives (x, y) num_lives = 
  let counter = ref 0 in
  let full_heart = Graphics.make_image Img_heart.full_heart in
  while !counter < num_lives do 
    Graphics.draw_image full_heart 
      (x + heart_size / 2 + 
       !counter * (Constant.heart_size + (Constant.heart_size / 4)))
      (y-Constant.heart_size);
    incr counter 
  done 

(* [reduce_hearts_img (x,y) loss num_lives] draws black heart for every life
   that is lost.
   [(x,y)] is the top left corner of the maze
   [loss] is the number of hearts lost
   [num_lives] is the current number of lives camel has.*)
let reduce_hearts_img (x, y) loss num_lives = 
  let empty_heart = Graphics.make_image Img_heart.empty_heart in 
  match loss with 
  | 0 -> ();
  | _ -> 
    Graphics.draw_image empty_heart 
      (x + heart_size / 2 + (num_lives) *
                            (Constant.heart_size + (Constant.heart_size / 4)))
      (y-Constant.heart_size)

(* [add_hearts_img (x,y) num_lives] draws red heart for every life
   that is gained.
   [(x,y)] is the top left corner of the maze
   [num_lives] is the current number of lives camel has.*)
let add_heart_img (x, y) num_lives = 
  let full_heart = Graphics.make_image Img_heart.full_heart in 
  Graphics.draw_image full_heart 
    (x + heart_size / 2 + (num_lives) *
                          (Constant.heart_size + (Constant.heart_size / 4)))
    (y-Constant.heart_size)

let draw_background ()= 
  let image = Graphics.make_image Background.background in 
  Graphics.draw_image image 0 0

let update_coin_value st coin_val = 
  let x,y = st.top_left_corner in 
  let posx, posy = (x + tile_width + heart_size/2,
                    y - (st.rows + 1) * tile_width + heart_size / 4) in 
  set_color 0x026144;
  fill_rect posx posy heart_size heart_size;
  set_color 0xffe524;
  moveto posx posy;
  set_font 
    "-b&h-lucidatypewriter-bold-r-normal-sans-17-120-100-100--0-iso8859-1";
  let coin_string = string_of_int coin_val in
  let coin_str = 
    match String.length coin_string with 
    | 1 -> "000" ^ coin_string
    | 2 -> "00" ^ coin_string
    | 3 -> "0" ^ coin_string
    | 4 -> coin_string
    | _ -> failwith "impossible"
  in
  draw_string coin_str

let update_time_left st time_left= 
  let x, y = st.top_left_corner in 
  let posx, posy = (x + tile_width * 4 - heart_size/4, 
                    y - (st.rows + 1) * tile_width + heart_size / 4) in 
  moveto posx posy;
  set_color 0x026144;
  fill_rect posx posy (tile_width * 5) heart_size;
  set_color 0xffe524;
  set_font 
    "-b&h-lucidatypewriter-bold-r-normal-sans-17-120-100-100--0-iso8859-1";
  if time_left >= 0 then 
    let sec = time_left mod 60 in
    let min = time_left / 60 in 
    draw_string  ((string_of_int min) ^ ":" ^ (string_of_int sec))
  else 
    draw_string "infinity"

let update_time_lapsed st time = 
  let x, y = st.top_left_corner in 
  let posx, posy = (x + (st.cols / 2 * tile_width) + heart_size * 2, 
                    y + heart_size/2) in
  let hour = time / 3600 in 
  let hour_str = if hour > 9 then string_of_int hour 
    else "0" ^ string_of_int hour 
  in 
  let min = time / 60 in 
  let min_str = if min > 9 then string_of_int min
    else "0" ^ string_of_int min 
  in 
  let sec = time mod 60 in 
  let sec_str = if sec > 9 then string_of_int sec 
    else "0" ^ string_of_int sec 
  in
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
  fill_rect (posx - 1) (posy - heart_size /3) 
    (tile_width * st.cols) (tile_width + heart_size/3);
  draw_image coin_img (posx + heart_size / 2) posy;
  update_coin_value st coin_val;
  draw_image (make_image hourglass) 
    (posx + tile_width * 3) (posy);
  update_time_left st (-1);
  initialize_lives (x, y + heart_size + 10) num_lives;
  moveto (x + (st.cols / 2 * tile_width) - 2 * tile_width) (y + heart_size/2);
  set_color 0xffe524;
  draw_string "TIME ELAPSED: ";
  update_time_lapsed st 0




let draw_element x y color size= 
  set_color color;
  fill_poly [|(x-1,y+1); (x+size,y+1); (x+size, y-size);(x-1, y-size)|]

let draw_walls (gen_maze : Maze.maze) start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - (i)*Constant.tile_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + (j)*Constant.tile_width, snd !curr_pos);
      let tile = tile_type gen_maze j i in
      if tile = Wall then begin
        let wall_img = make_image sand_wall in 
        draw_image wall_img (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      end 
      else if tile = Start then begin 
        let start_tile = make_image portal_tile in 
        draw_image start_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      end
      else if tile = Exit then begin
        let end_tile = make_image portal_tile2 in 
        draw_image end_tile (fst !curr_pos - 1) (snd !curr_pos - tile_width);
      end
      else
        let path_img = make_image sand_tile2 in 
        draw_image path_img (fst !curr_pos - 1) (snd !curr_pos - tile_width);
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
    | _ -> failwith "impossible"
  in 
  draw_image camel_pic (x - camel_radius) (y - camel_radius)

let draw_enemy (enemy : Enemy.t) = 
  (* set_color Constant.enemy_color;  *)
  let enemy_pic = 
    match enemy.dir with 
    | 0 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_0
    | 90 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_90
    | 180 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_180
    | 270 -> Graphics.make_image Img_enemy_camel.enemy_camel_pic_270
    | _ -> failwith "impossible"
  in
  let (x, y) = (enemy.pos.x, enemy.pos.y) in 
  Graphics.draw_image enemy_pic 
    (x - Constant.camel_radius) (y - Constant.camel_radius)

let draw_coin (coin : Coin.t) =
  let (x, y) = (coin.pos.x, coin.pos.y) in 
  let coin_img = make_image coin_pic in 
  draw_image coin_img (x - coin_radius) (y - coin_radius)

let draw_projectile (proj : Projectile.t) =
  Graphics.set_color Constant.projectile_color;
  let (x, y) = (proj.pos.x, proj.pos.y) in 
  fill_poly [|(x-projectile_radius, y+projectile_radius); 
              (x+projectile_radius, y+projectile_radius); 
              (x+projectile_radius, y-projectile_radius); 
              (x-projectile_radius, y-projectile_radius)|]

let draw_round_state (st : Round_state.t) = 
  draw_maze st;
  draw_camel st.camel; 
  Array.iter draw_enemy st.enemies; 
  List.iter draw_projectile st.projectiles;
  Array.iter draw_coin st.coins;
  Graphics.synchronize ()

let draw_welcome () = 
  Graphics.set_window_title "Skedaddle Camel";
  draw_background ();
  Graphics.set_text_size 300;
  Graphics.moveto 20 700;
  Graphics.draw_string "Welcome to Skedaddle Camel!";
  Graphics.moveto 20 650; 
  Graphics.draw_string "The goal of the game is to navigate a series of mazes.";
  Graphics.moveto 20 635;
  Graphics.draw_string "The beginning of the maze is the blue square in the upper left corner; exit is the bottom right.";
  Graphics.moveto 20 600; 
  Graphics.draw_string "Use WASD to move and press space to shoot projectiles.";
  Graphics.moveto 20 585;
  Graphics.draw_string "The projectiles will be shot in the same direction you are going.";
  Graphics.moveto 20 550; 
  Graphics.draw_string "Avoid enemies! If you get too close, you die :( ";
  Graphics.moveto 20 500; 
  Graphics.set_color Graphics.red; 
  Graphics.draw_string "Choose your level of difficulty to start playing! (default: easy)";
  Graphics.moveto 20 485; 
  Graphics.draw_string "Press `1` for easy, press `2` for hard";
  Graphics.set_color Graphics.black;
  Graphics.synchronize ()

let draw_finscore (gs : Game_state.game_state) = 
  let st = gs.round_state in 
  let scr = gs.score in 
  let coins = scr.coins in 
  let difficulty = 
    match gs.game_difficulty with 
    | Easy -> "Easy"
    | Hard -> "Hard"
  in 
  let x, y = (fst st.top_left_corner, 
              snd st.top_left_corner - 25) in
  Graphics.moveto x y;
  Graphics.draw_string ("Game difficulty: " ^ difficulty);
  Graphics.moveto x (y - 25);
  Graphics.draw_string ("Enemies killed: " ^ string_of_int scr.hit);
  Graphics.moveto x (y - 50);
  Graphics.draw_string ("Coins collected: " ^ string_of_int coins);
  Graphics.moveto x (y - 75);
  let calculated_score = 
    match gs.game_difficulty with 
    | Easy -> Scorer.score scr st.camel false
    | Hard -> Scorer.score scr st.camel true 
  in 
  Graphics.draw_string ("Final score: " ^ 
                        string_of_int calculated_score);
  Graphics.synchronize ()

let draw_transition (t : int) (gs : Game_state.game_state) : unit = 
  draw_background ();
  let st = gs.round_state in 
  let x, y = (fst st.top_left_corner, snd st.top_left_corner - 10) in
  Graphics.moveto x y;
  Graphics.set_text_size 300; 
  Graphics.set_color Graphics.blue;
  Graphics.draw_string ("Welcome to level " ^ string_of_int (t + 1));
  let y = y - 25 in Graphics.moveto x y;
  Graphics.draw_string "(Press `x` to start the next level)";
  Graphics.set_color Graphics.black;
  let calculated_score = if t = 0 then 0 else 
      match gs.game_difficulty with 
      | Easy -> Scorer.score gs.score st.camel false
      | Hard -> Scorer.score gs.score st.camel true 
  in 
  let y = y - 25 in Graphics.moveto (x + 25) y;
  Graphics.draw_string ("Score so far: " ^ 
                        string_of_int calculated_score);
  (* Graphics.synchronize (); *)
  let y = y - 25 in Graphics.moveto x y;
  let _ = match t with 
    | 0 -> Graphics.draw_string "This level has 0 enemies";
    | 1 -> Graphics.draw_string "This level has 2 enemies";
    | 2 -> Graphics.draw_string "This level has 10 enemies";
    | _ -> ();
  in 
  let _ = match gs.game_difficulty with 
    | Easy -> ();
    | Hard -> begin
        let y = y - 25 in Graphics.moveto x y;
        match t with 
        | 0 -> Graphics.draw_string "There is no time limit";
        | 1 -> Graphics.draw_string 
                 "You have 100 seconds to escape this level!";
        | 2 -> Graphics.draw_string "You have 60 seconds to escape this level!";
        | _ -> ();
      end 
  in Graphics.synchronize () 

let draw_gameover (gs : Game_state.game_state) (over : Game_state.game_end) = 
  draw_background ();
  let msg = 
    match over with 
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
  (* let x, y = (200 + fst st.top_left_corner, (snd st.top_left_corner)+10) in
     Graphics.moveto x y;
     Graphics.set_text_size 300; 
     Graphics.set_color 0xB03F37;
     Graphics.fill_rect (x-10) (y-5) 110 20;
     Graphics.set_color Graphics.white; 
     Graphics.draw_string ("TIME ELAPSED: " ^ 
                        string_of_int (timer.elapsedtime)); *)
  match gs.game_difficulty with 
  | Easy -> ()
  | Hard -> begin 
      let curr_round = 
        if gs.score.mazes = 0 then Constant.round1 
        else if gs.score.mazes = 1 then Constant.round2 
        else Constant.round3 in 
      match Timer.time_left curr_round timer with 
      | None -> () 
      | Some time -> begin 
          update_time_left gs.round_state time;
        end 
    end

let rec draw_words height (pos : Position.t) = function
  | [] -> ()
  | h :: t -> 
    Graphics.moveto pos.x pos.y;
    Graphics.draw_string h;
    draw_words height (Position.init_pos (pos.x, pos.y-height)) t

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
  | Welcome -> draw_welcome ();
  | InPlay -> draw_round_state gs.round_state; 
    draw_level_num gs; 
    let x = fst st.top_left_corner in 
    Graphics.moveto (x + 10) 10;
    Graphics.set_text_size 50;
    Graphics.set_color 0xB03F37;
    update_coin_value st (gs.score.coins + gs.round_state.camel.coins);
    Graphics.set_color 0xFCF25D;
    reduce_hearts_img (x, snd st.top_left_corner + heart_size + 10) 
      (num_lives - st.camel.health) st.camel.health;
    draw_time gs timer
  | Transition t -> draw_transition t gs 
  | Won -> draw_won gs
  | GameOver over -> draw_gameover gs over



