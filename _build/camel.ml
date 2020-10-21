open Position 

type t = {
  pos : Position.t;
  dir : int; (* direction in degrees *)
  health : int;
  coins : int;
}

let speed = 5. (* distance camel travels on one key press *)
let rot = 90 (* degrees camel rotates on one key press *)

let init x y = 
  {pos = {x = x; y = y}; dir = 0; health = 3; coins = 0}

let turn_right camel = 
  {camel with dir = (camel.dir - rot + 360) mod 360}

let turn_left camel = 
  {camel with dir = (camel.dir + rot) mod 360}

let move_horiz camel sign = 
  {camel with pos = {x = camel.pos.x +. (sign *. speed); y = camel.pos.y}}

let move_vert camel sign = 
  {camel with pos = {x = camel.pos.x; y = camel.pos.y +. (sign *. speed)}}

let adj_health camel h = 
  {camel with health = camel.health + h}

let string_of_camel camel = 
  "Camel health: " ^ string_of_int camel.health ^ 
  "\n" ^ "Camel position: " ^ Position.string_of_pos camel.pos ^
  "\n" ^ "Camel direction: " ^ string_of_int camel.dir