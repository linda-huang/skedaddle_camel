open Graphics;;
open Maze;;
(*Use x y to refer to pixel posiion
  use row col for maze indexing*)
let default_x = 600
let default_y = 450

let gen_maze = populate 10 10 (0,0)

let maze_row = Array.length gen_maze
let maze_col = Array.length gen_maze.(0)

let path_width = 25

let start_y = default_y - ((default_y - maze_row * path_width) / 2)
let start_x = ((default_x - maze_col * path_width) / 2)

let start_pos = (start_x, start_y)

let draw_element x y color = 
  set_color color;
  fill_poly [|(x,y); (x+25,y); (x+25, y-25); (x, y-25)|]

(* note that the current_point of the graphics library is 0,0 *)
(* let draw_maze_frame () = 
   moveto (fst start_pos) (snd start_pos);
   lineto ((fst start_pos) + maze_col*Maze.path_width) (snd start_pos);
   lineto (fst (current_point ())) ((snd (current_point ())) - maze_row*Maze.path_width);
   lineto ((fst (current_point ())) - maze_col*Maze.path_width) (snd (current_point ()));
   lineto (fst start_pos) (snd start_pos) *)

let draw_walls gen_maze = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i*path_width);
    for j = 0 to maze_col - 1 do begin    
      curr_pos := ((fst start_pos) + (j)*path_width , snd !curr_pos);
      if isWall gen_maze i j then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.black
      end
      else
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.green;
      (* Printf.printf "updating this is i %d" i;
         print_newline;
         Printf.printf "this is j %d" j;
         print_newline;
         Printf.printf "this is updated curr pos i %d" (fst !curr_pos);
         print_newline;
         Printf.printf "this is updated curr pos j %d" (snd !curr_pos);
         print_newline; *)
    end

    done
  end
  done 

let main gen_maze = 
  Graphics.open_graph "";
  set_window_title "Camel Maze";
  moveto 230 425;
  Graphics.set_text_size 300; 
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  (* Graphics.draw_string "PRESS ANY KEY TO PLAY"; *)
  (* draw_maze_frame (); *)
  draw_walls gen_maze

let () = main ()

(* let draw_maze_frame () = 
   moveto (start_x) (start_y);
   lineto (start_x + maze_row*25) (start_y);
   lineto (fst (current_point ())) ((snd (current_point ())) - maze_col*25);
   lineto ((fst (current_point ())) - maze_row*25) (snd (current_point ()));
   lineto (start_x) (start_y) *)



