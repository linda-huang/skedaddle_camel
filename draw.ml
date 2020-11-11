open Graphics
open Camel
open Enemy
open Maze 
open Round_state
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
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  draw_walls st.maze start_pos st.rows st.cols

let draw_camel (camel : Camel.t) = 
  let color = Constant.camel_color in 
  set_color color; 
  let (x, y) = (camel.pos.x, camel.pos.y) in 
  fill_poly [|(x-camel_radius,y+camel_radius); 
              (x+camel_radius,y+camel_radius); 
              (x+camel_radius, y-camel_radius); 
              (x-camel_radius, y-camel_radius)|]

let draw_enemy (enemy : Enemy.t) = 
  let color = Constant.enemy_color in 
  set_color color; 
  let (x, y) = (enemy.pos.x, enemy.pos.y) in 
  fill_poly [|(x-camel_radius,y+camel_radius); 
              (x+camel_radius,y+camel_radius); 
              (x+camel_radius, y-camel_radius); 
              (x-camel_radius, y-camel_radius)|]

let draw_coin (coin : Coin.t) =
  let color = Graphics.rgb 171 149 7 in 
  set_color color;
  let (x, y) = (coin.pos.x, coin.pos.y) in 
  fill_poly [|(x-coin_radius,y+coin_radius); 
              (x+coin_radius,y+coin_radius); 
              (x+coin_radius, y-coin_radius); 
              (x-coin_radius, y-coin_radius)|]

let draw_projectile (proj : Projectile.t) =
  Graphics.rgb 79 212 219 |> set_color ; (* light blue *)
  let (x, y) = (proj.pos.x, proj.pos.y) in 
  fill_poly [|(x-projectile_radius,y+projectile_radius); 
              (x+projectile_radius,y+projectile_radius); 
              (x+projectile_radius, y-projectile_radius); 
              (x-projectile_radius, y-projectile_radius)|];
  Graphics.moveto (x - 50) (y - 50);
  Graphics.draw_string (string_of_int proj.dir)

let draw_round_state (st : Round_state.t) = 
  draw_maze st;
  draw_camel st.camel; 
  Array.iter draw_enemy st.enemies; 
  List.iter draw_projectile st.projectiles;
  Graphics.synchronize ();
  ()
