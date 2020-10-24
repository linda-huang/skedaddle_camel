open Graphics
open Maze
open Unix 

let draw_element x y color = 
  set_color color;
  fill_poly [|(x,y); (x+path_width,y); (x+path_width, y-path_width); 
              (x, y-path_width)|]

let draw_walls gen_maze start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i*path_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + (j)*path_width , snd !curr_pos);
      if tile_type gen_maze j i = Wall then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.black;
      end
      else
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.green;
    end
    done
  end
  done 

let main m n = 
  let maze_row = m in
  let maze_col = n in
  let window_height = maze_row * path_width + 200 in
  let window_width = maze_col * path_width + 200 in
  Graphics.open_graph (" " ^ (string_of_int window_width) ^ "x" ^ 
                       (string_of_int window_height));
  set_window_title "Camel Maze";
  moveto (window_width / 2 - 55) (window_height - 50);
  Graphics.set_text_size 300; 
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  let gen_maze = populate maze_row maze_col (0,0) in
  let start_y = window_height - ((window_height- maze_row * path_width) / 2) in
  let start_x = ((window_width - maze_col * path_width) / 2) in
  let start_pos = (start_x, start_y) in
  draw_walls gen_maze start_pos maze_row maze_col;
  Unix.sleep 100

let () = main 45 61

