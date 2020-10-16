open Camel
open Enemy
open Maze 
open Graphics     

let start x y = 
  Camel.init x y 

let input_move camel = 
  if (Graphics.key_pressed ()) then 
    let k = Graphics.read_key () in 
    flush_kp ();
    match k with 
    | 'w' -> Camel.move_vert camel 1.
    | 'a' -> Camel.move_horiz camel -1.
    | 's' -> Camel.move_vert camel -1.
    | 'd' -> Camel.move_horiz camel 1.
    | 'q' -> Camel.turn_left camel
    | 'e' -> Camel.turn_right camel 
    | _ -> camel 


