open Graphics

let rec run () =
  Graphics.auto_synchronize false;
  Graphics.clear_graph ();


  Graphics.moveto 50 50;
  Graphics.draw_string "press a key to move (press 0 to exit)";
  Graphics.moveto 50 250;
  Graphics.draw_string (string_of_float (Sys.time ()));
  Graphics.auto_synchronize true;
  run ()


let main () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.resize_window 600 800;
  Graphics.set_text_size 300;
  Graphics.moveto 20 100;
  Graphics.draw_string "press any key to start";
  run ()

(* Execute the demo. *)
let () = main ()