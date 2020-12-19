(** Game-wide constants relating to movement, display, 
    and inter-agent interactions *)

(** The type storing constants about each round
    e.g. the size of maze and number of enemies *)
type round_info = {
  dimx : int;
  dimy : int;
  enemies : int;
  timelim : int;
  portals : int
}

(** [round1] is the information for the first level *)
val round1 : round_info

(** [round2] is the information for the second level *)
val round2 : round_info

(** [round3] is the information for the third level *)
val round3 : round_info

(** [totrounds] is the total number of levels in the game *)
val totrounds : int 

(** [camel_speed] is the distance (in pixels) camel travels on one key press *)
val camel_speed : int

(** [camel_mud_speed] is the distance (in pixels) camel travels on one key press 
    on muddy tiles*)
val camel_mud_speed : int

(** [camel_rot] is the number of degrees the camel rotates on one key press *)
val camel_rot : int

(** [camel_width] is the pixel width of the camel *)
val camel_width : int

(** [camel_radius] is half of [camel_width] *)
val camel_radius : int

(** [camel_color] is the color of camel *)
val camel_color : Graphics.color

(** [enemy_speed] is the distance (in pixels) that an enemy
    travels on one unit of game time *)
val enemy_speed : int

(** [enemy_color] is the color of enemy camels *)
val enemy_color : Graphics.color

(** [health_delay] is the time in ms that health cannot be decremented after 
    camel loses one unit of health *)
val health_delay : float 

(** [genie_width] is the pixel width of the genie *)
val genie_width : int

(** [genie_radius] is half of [genie_width] *)
val genie_radius : int

(** [genie_speed] is the distance (in pixels) that a genie
    travels on one unit of game time *)
val genie_speed : int 

(** [genie_power] is the amount that the genie increases the 
    camel's coin count by *)
val genie_power : int 

(** [genie_color] is the Graphics color code for a solid Genie icon *)
val genie_color : Graphics.color

(** [genie_teleport_time] is how often (in seconds) a genie teleports *)
val genie_teleport_time : float 

(** [hourglass_width] is the pixel width of the hourglass *)
val hourglass_width : int

(** [hourglass_radius] is half of [hourglass_width] *)
val hourglass_radius : int

(** [hourglass_power] is the amount that the hourglass increases the 
    remaining time by *)
val hourglass_add : int 

(** [hourglass_add_color] is the Graphics color code for a solid hourglass icon
    that has the add time power *)
val hourglass_add_color : Graphics.color

(** [hourglass_pause_color] is the Graphics color code for a solid hourglass icon
    that has the pause power *)
val hourglass_pause_color : Graphics.color

(* * [draw_tile_width] is the pixel width of a maze tile for drawing
   val draw_tile_width : int *)

(** [real_tile_width] is the actual number of pixels that is the width of the 
    tile*)
val tile_width : int 

(** [tile_radius] is half of [tile_width] *)
val tile_radius : int

(** [path_color] is the Graphics color code for a solid path tile *)
val path_color : Graphics.color

(** [path_pic] is the Graphics representation of the image 
    for a path tile in the maze *)
val path_pic : Graphics.color array array

(** [wall_health] is the amount of shots it takes to remove a wall 
    (convert a wall tile to a path tile) *)
val wall_health : int 

(** [wall_color] is the Graphics color code for a solid wall tile *)
val wall_color : Graphics.color

(** [wall_pic] is the Graphics representation of the image 
    for a wall tile in the maze *)
val wall_pic : Graphics.color array array

(** [start_color] is the Graphics color code for a solid start tile *)
val start_color : Graphics.color

(** [exit_color] is the Graphics color code for a solid exit tile *)
val exit_color : Graphics.color

(** [ice_color] is the Graphics color code for a solid ice tile *)
val ice_color : Graphics.color

(** [mud_color] is the Graphics color code for a solid mud tile *)
val mud_color : Graphics.color

(** [portal_color] is the Graphics color code for a solid portal tile *)
val portal_color : Graphics.color

(** [near] is how far two center points of agents must be to be 
    considered a hit *)
val near : int

(** [potion_width] is the pixel width of a potion *)
val potion_width : int

(** [potion_radius] is half of [potion_width] *)
val potion_radius : int

(** [potion_color] is the Graphics color code for a solid potion pixel icon *)
val potion_color : Graphics.color

(** [coin_width] is the pixel width of a coin *)
val coin_width : int

(** [coin_radius] is half of [coin_width] *)
val coin_radius : int

(** [coin_color] is the Graphics color code for a solid coin pixel icon *)
val coin_color : Graphics.color

(** [coin_pic] is the Graphics representation of the image 
    for a coin in the maze *)
val coin_pic : Graphics.color array array

(** [projectile_width] is the pixel width of a projectile *)
val projectile_width : int

(** [projectile_radius] is half of [projectile_width] *)
val projectile_radius : int

(** [projectile_color] is the Graphics color code
    for a solid projectile pixel icon *)
val projectile_color : Graphics.color 

(** [projectile_speed] is the distance (in pixels) that an enemy
    travels on one unit of game time *)
val projectile_speed : int

(** [camel_0] is the Graphics representation of the image 
    for a camel moving in direction 0 (right) *)
val camel_0 : Graphics.color array array

(** [camel_90] is the Graphics representation of the image 
    for a camel moving in direction 90 (up) *)
val camel_90 : Graphics.color array array

(** [camel_180] is the Graphics representation of the image 
    for a camel moving in direction 180 (left) *)
val camel_180 : Graphics.color array array

(** [camel_270] is the Graphics representation of the image 
    for a camel moving in direction 270 (down) *)
val camel_270 : Graphics.color array array