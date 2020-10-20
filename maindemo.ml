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
  | _ -> camel 

let rec run (camel : Camel.t) = 
  Graphics.clear_graph ();
  Graphics.moveto 50 500;
  let movedcamel = input camel in 
  Graphics.draw_string ("Began as: " ^ Camel.string_of_camel camel);
  Graphics.draw_string ("\nMoved to: " ^ Camel.string_of_camel movedcamel);
  Unix.sleep 1;
  run movedcamel 

let init k = 
  let camel = Camel.init 0. 0. in 
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