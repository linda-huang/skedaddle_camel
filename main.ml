open Camel
open Graphics     

let input_move camel = 
  if (Graphics.key_pressed ()) then 
    let k = Graphics.read_key () in 
    flush_kp ();


