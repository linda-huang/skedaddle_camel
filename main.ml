open Graphics
open Camel
open Enemy
open Maze 
open State      

let start x y = 
  Camel.init x y 

let try_move mz orig moved = 
  if State.hit_wall moved.pos mz then moved else orig 

let input_move camel mz = 
  if (Graphics.key_pressed ()) then 
    let k = Graphics.read_key () in 
    let try_move' = try_move mz camel in 
    match k with 
    | 'w' -> try_move' (Camel.move_vert camel 1.)
    | 'a' -> try_move' (Camel.move_horiz camel ~-.1.)
    | 's' -> try_move' (Camel.move_vert camel ~-.1.)
    | 'd' -> try_move' (Camel.move_horiz camel 1.)
    | 'q' -> try_move' (Camel.turn_left camel)
    | 'e' -> try_move' (Camel.turn_right camel) 
    (* | '.' -> State.shoot camel *)
    | _ -> camel 
  else camel 

let is_dead camel = camel.health = 0

(* updates health and coin total of camel *)
let update_state camel st = 
  let newcamel = 
    if (State.near_enemy camel st) then 
      {camel with health = camel.health - 1} else 
    if (State.on_coin camel st) then 
      {camel with coins = camel.coins + 1} else camel in 
  (* let newstate = 
     if (State.on_coin camel st) then failwith "todo: remove coin from coinlist"
         {st with coins = }*)
  if (is_dead newcamel) then failwith " game over ??? " else newcamel  


(* Graphics.draw_image : image -> int -> int -> unit
   Draw the given image with lower left corner at the given point.*)