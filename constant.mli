(** Game-wide constants relating to movement, display, 
    and inter-agent interactions *)

(** [camel_speed] is the distance (in pixels) camel travels on one key press *)
val camel_speed : int

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

(** [tile_width] is the pixel width of a maze tile *)
val tile_width : int

(** [tile_radius] is half of [tile_width] *)
val tile_radius : int

(** [path_color] is the Graphics color code for a solid path tile *)
val path_color : Graphics.color

(** [path_pic] is the Graphics representation of the image 
    for a path tile in the maze *)
val path_pic : Graphics.color array array

(** [wall_color] is the Graphics color code for a solid wall tile *)
val wall_color : Graphics.color

(** [wall_pic] is the Graphics representation of the image 
    for a wall tile in the maze *)
val wall_pic : Graphics.color array array

(** [start_color] is the Graphics color code for a solid start tile *)
val start_color : Graphics.color

(** [exit_color] is the Graphics color code for a solid exit tile *)
val exit_color : Graphics.color

(** [near] is how far two center points of agents must be to be 
    considered a hit *)
val near : int

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