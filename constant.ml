(**********************************************************
   Camel constants 
 ***********************************************************)
(* [speed] is the distance camel travels on one key press *)
let camel_speed = 5
(* [rot] is the number of degrees the camel rotates on one key press *)
let camel_rot = 90 
let camel_width = 15
let camel_radius = camel_width / 2
let camel_color = Graphics.rgb 255 111 97
let enemy_color = Graphics.rgb 179 27 27

(**********************************************************
   Maze constants
 ***********************************************************)
let tile_width = 25
let tile_radius = tile_width / 2
let near = 5 

let path_color = Graphics.rgb 255 248 220
let wall_color = Graphics.rgb 136 176 75
let exit_color = Graphics.rgb 146 168 209
let start_color = Graphics.rgb 146 168 209

(**********************************************************
   Coin constants
 ***********************************************************)
let coin_width = 4
let coin_radius = coin_width / 2
let coin_color = Graphics.rgb 171 149 7

(**********************************************************
   Projectile constants
 ***********************************************************)
let projectile_width = 6
let projectile_radius = projectile_width / 2
let projectile_color = Graphics.rgb 79 212 219