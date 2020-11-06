open Graphics
open Camel
open Enemy
open Maze 
open State
open Scorer


let rec run1 st scr =
  Graphics.auto_synchronize false;
  Graphics.clear_graph ();


  Graphics.moveto 50 50;
  Graphics.draw_string "press a key to move (press 0 to exit)";
  Graphics.moveto 50 250;
  Graphics.draw_string (string_of_float (Sys.time ()));
  Graphics.auto_synchronize true;
  run1 st scr

let init1 () =
  let camel = Camel.init 0. 0. in 
  let st = State.init_test camel 45 45 5 Maindemo.fightingring in 
  let scr = Scorer.init () in 
  Maindemo.draw_state st; 
  Graphics.moveto 20 700;
  run1 st scr

let main1 () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.resize_window 1600 900;
  Graphics.set_text_size 300;
  Graphics.moveto 20 100;
  Graphics.draw_string "TEST: press any key to start";
  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then init1 ()

(* Execute the demo. *)
let () = main1 ()