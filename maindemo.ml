open Graphics
open Camel
open Enemy
open Maze 
open State   
open Projectile

let draw_state state = failwith "todo" 

let init k =  
  Graphics.clear_graph ();
  let camel = Camel.init 0. 0. in 
  let movedcamel = 
    match k with 
    | 'w' -> (Camel.move_vert camel 1.)
    | 'a' -> (Camel.move_horiz camel ~-.1.)
    | 's' -> (Camel.move_vert camel ~-.1.)
    | 'd' -> (Camel.move_horiz camel 1.)
    | _ -> camel 
  in Graphics.draw_string (Camel.string_of_camel movedcamel)

(*draw_state State.init_state; run State.init_state*)

let main () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.draw_string "press a key to move";
  match Graphics.read_key () with 
  | k -> init k 
(* Execute the game engine. *)
let () = main () 