open Position 
open Constant

type t = {
  pos : Position.t; (* center coordinates of camel *)
  dir : int; (* direction in degrees *)
  health : int;
  coins : int;
}

let init x y = 
  {pos = {x = x; y = y}; dir = 0; health = 3; coins = 0}

let turn_right camel = 
  {camel with dir = (camel.dir - Constant.camel_rot + 360) mod 360}

let turn_left camel = 
  {camel with dir = (camel.dir + Constant.camel_rot) mod 360}

let move_horiz camel sign = 
  {camel with pos = {x = camel.pos.x + (sign * Constant.camel_speed); y = camel.pos.y}}

let move_vert camel sign = 
  {camel with pos = {x = camel.pos.x; y = camel.pos.y + (sign * Constant.camel_speed)}}

let adj_health camel h = 
  {camel with health = camel.health + h}

let adj_coin camel v = 
  {camel with coins = camel.coins + v}

let is_dead camel = camel.health = 0

let string_of_camel camel = 
  "Camel health: " ^ string_of_int camel.health ^ 
  "\n" ^ "Camel position: " ^ Position.string_of_pos camel.pos ^
  "\n" ^ "Camel direction: " ^ string_of_int camel.dir