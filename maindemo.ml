open Graphics
open Camel
open Enemy
open Maze 
open State   
open Projectile
open Unix 

let draw_state state = failwith "todo" 

let input (camel : Camel.t) =  
  Graphics.clear_graph ();
  Graphics.moveto 50 500;
  Graphics.draw_string "press a key to move";
  let k = Graphics.read_key () in 
  match k with 
  | '0' -> exit 0 
  | 'w' -> (Camel.move_vert camel 1.)
  | 'a' -> (Camel.move_horiz camel ~-.1.)
  | 's' -> (Camel.move_vert camel ~-.1.)
  | 'd' -> (Camel.move_horiz camel 1.)
  | 'e' -> (Camel.turn_right camel)
  | 'q' -> (Camel.turn_left camel)
  | _ -> camel 

let rec run (camel : Camel.t) = 
  Graphics.clear_graph ();
  Graphics.moveto 50 500;
  let movedcamel = input camel in 
  Graphics.moveto 50 400;
  Graphics.draw_string ("Began as: " ^ Camel.string_of_camel camel);
  Graphics.moveto 50 300;
  Graphics.draw_string ("Moved to: " ^ Camel.string_of_camel movedcamel);
  Unix.sleep 3;
  run movedcamel 

let init k = 
  let camel = Camel.init 0. 0. in 
  (*let maze = Maze.populate 10 10 (0, 0) in 
    let enemies = State.init_enemy_lst 5 in *)
  run camel 

(*draw_state State.init_state; run State.init_state*)

let main () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.resize_window 800 800;
  Graphics.set_text_size 300;
  Graphics.moveto 50 500;
  Graphics.draw_string "press a key to start";
  match Graphics.read_key () with 
  | k -> init k 

(* Execute the demo. *)
let () = main ()
(*https://stackoverflow.com/questions/36263152/simple-ocaml-graphics-progam-that-close-before-its-window-is-displayed *)