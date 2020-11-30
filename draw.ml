open Graphics
open Camel
open Enemy
open Maze 
open Round_state
open Game_state 
open Scorer 
open Coin
open Constant 

let draw_element x y color size= 
  set_color color;
  fill_poly [|(x,y); (x+size,y); (x+size, y- size); 
              (x, y-size)|]

let draw_walls (gen_maze : Maze.maze) start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i*Constant.tile_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + (j)*Constant.tile_width , snd !curr_pos);
      let tile = tile_type gen_maze j i in
      if tile = Wall then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Constant.wall_color
          Constant.tile_width;
      end 
      else if tile = Start then begin 
        draw_element (fst !curr_pos) (snd !curr_pos) Constant.start_color
          Constant.tile_width;
      end
      else if tile = Exit then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Constant.exit_color
          Constant.tile_width;
      end
      else
        draw_element (fst !curr_pos) (snd !curr_pos) Constant.path_color
          Constant.tile_width;
    end
    done
  end
  done 

let draw_maze (st : Round_state.t) = 
  let start_pos = (fst st.top_left_corner, snd st.top_left_corner) in
  moveto (fst start_pos) (snd start_pos);
  Graphics.set_text_size 300; 
  Graphics.set_color Graphics.black;
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  draw_walls st.maze start_pos st.rows st.cols

let draw_camel (camel : Camel.t) = 
  set_color Constant.camel_color; 
  let (x, y) = (camel.pos.x, camel.pos.y) in 
  fill_poly [|(x-camel_radius,y+camel_radius); 
              (x+camel_radius,y+camel_radius); 
              (x+camel_radius, y-camel_radius); 
              (x-camel_radius, y-camel_radius)|]

let draw_enemy (enemy : Enemy.t) = 
  set_color Constant.enemy_color; 
  let (x, y) = (enemy.pos.x, enemy.pos.y) in 
  fill_poly [|(x-camel_radius,y+camel_radius); 
              (x+camel_radius,y+camel_radius); 
              (x+camel_radius, y-camel_radius); 
              (x-camel_radius, y-camel_radius)|]

let draw_coin (coin : Coin.t) =
  set_color Constant.coin_color;
  let (x, y) = (coin.pos.x, coin.pos.y) in 
  fill_poly [|(x-coin_radius,y+coin_radius); 
              (x+coin_radius,y+coin_radius); 
              (x+coin_radius, y-coin_radius); 
              (x-coin_radius, y-coin_radius)|]

let draw_projectile (proj : Projectile.t) =
  set_color Constant.projectile_color;
  let (x, y) = (proj.pos.x, proj.pos.y) in 
  fill_poly [|(x-projectile_radius,y+projectile_radius); 
              (x+projectile_radius,y+projectile_radius); 
              (x+projectile_radius, y-projectile_radius); 
              (x-projectile_radius, y-projectile_radius)|];
  ()

let draw_round_state (st : Round_state.t) = 
  draw_maze st;
  draw_camel st.camel; 
  Array.iter draw_enemy st.enemies; 
  List.iter draw_projectile st.projectiles;
  Array.iter draw_coin st.coins;
  Graphics.synchronize ()

let draw_welcome () = 
  Graphics.set_window_title "Skedaddle Camel";
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
  Graphics.draw_string "Press any key to start";
  Graphics.synchronize ()

let draw_finscore (st : Round_state.t) (scr : Scorer.t) = 
  let coins = scr.coins in 
  let start_pos = (fst st.top_left_corner, snd st.top_left_corner) in
  let x = fst start_pos in 
  let y = snd start_pos in 
  Graphics.moveto x (y - 25);
  Graphics.draw_string ("Enemies killed: " ^ string_of_int scr.hit);
  Graphics.moveto x (y - 50);
  Graphics.draw_string ("Coins collected: " ^ string_of_int coins);
  Graphics.moveto x (y - 75);
  Graphics.draw_string ("Final score: " ^ 
                        string_of_int (Scorer.score scr st.camel));
  Graphics.synchronize ()

let draw_gameover (gs : Game_state.game_state) : unit = 
  Graphics.clear_graph ();
  let start_pos = (fst gs.round_state.top_left_corner, 
                   snd gs.round_state.top_left_corner) in
  let x = fst start_pos in 
  let y = snd start_pos in 
  Graphics.moveto x y;
  Graphics.draw_string "Game ended!";
  draw_finscore gs.round_state gs.score; 
  Graphics.synchronize ();
  match Graphics.read_key () with 
  | '0' -> exit 0
  | _ -> ()

let draw_won (gs : Game_state.game_state) : unit = 
  Graphics.clear_graph ();
  let start_pos = (fst gs.round_state.top_left_corner, 
                   snd gs.round_state.top_left_corner) in
  let x = fst start_pos in 
  let y = snd start_pos in 
  Graphics.moveto x y;
  Graphics.draw_string "Congratulations! You've escaped!";
  draw_finscore gs.round_state gs.score; 
  Graphics.synchronize ();
  let s = wait_next_event[Key_pressed] in
  if s.keypressed then 
    match Graphics.read_key () with 
    | '0' -> exit 0
    | _ -> ()

let draw_game_state (gs : Game_state.game_state) = 
  match gs.current_state with 
  | Welcome -> draw_welcome ()
  | InPlay -> draw_round_state gs.round_state
  | Won -> draw_won gs
  | GameOver -> draw_gameover gs
