open Graphics;;
open Maze;;

let default_x = 600
let default_y = 450

let gen_maze = Maze.populate 10 10 (0,0)

let maze_x = Array.length gen_maze
let maze_y = Array.length gen_maze.(0)

let path_width = 25

let start_x = maze_x * path_width / 2
let start_y = default_y - ((default_y - maze_y * path_width) / 2)

let start_pos = (start_x, start_y)

let draw_element x y color = 
  fill_poly [|(x,y); (x+25,y); (x+25, y-25); (x, y-25)|]


(* note that the current_point of the graphics library is 0,0 *)
let draw_maze_frame () = 
  moveto (fst start_pos) (snd start_pos);
  lineto ((fst start_pos) + maze_x*Maze.path_width) (snd start_pos);
  lineto (fst (current_point ())) ((snd (current_point ())) - maze_y*Maze.path_width);
  lineto ((fst (current_point ())) - maze_x*Maze.path_width) (snd (current_point ()));
  lineto (fst start_pos) (snd start_pos)

let draw_walls gen_maze = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_x - 1 do
    for j = 0 to maze_y - 1 do begin    
      if isWall gen_maze i j then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.black;
        curr_pos := ((fst start_pos) + j*path_width , (snd start_pos) - i*path_width)
      end
      else ()
    end   
    done
  done 

let main gen_maze = 
  Graphics.open_graph "";
  set_window_title "Camel Maze";
  moveto 200 400;
  Graphics.set_text_size 300; 
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  (* Graphics.draw_string "PRESS ANY KEY TO PLAY"; *)
  draw_maze_frame ();
  draw_walls gen_maze

let () = main ()

let draw_maze_frame () = 
  moveto (fst start_pos) (snd start_pos);
  lineto ((fst start_pos) + maze_x*25) (snd start_pos);
  lineto (fst (current_point ())) ((snd (current_point ())) - maze_y*25);
  lineto ((fst (current_point ())) - maze_x*25) (snd (current_point ()));
  lineto (fst start_pos) (snd start_pos)