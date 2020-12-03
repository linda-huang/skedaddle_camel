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
(** [enemy_color] is the color of enemy camels *)
val enemy_color : Graphics.color
(** [health_delay] is the time in ms that health cannot be decremented after 
    camel loses one unit of health *)
val health_delay : float 


val tile_width : int
val tile_radius : int
val path_color : Graphics.color
val wall_color : Graphics.color
val start_color : Graphics.color
val exit_color : Graphics.color

(** [near] is how far two center points of agents must be to be 
    considered a hit *)
val near : int

val coin_width : int
val coin_radius : int
val coin_color : Graphics.color

val projectile_width : int
val projectile_radius : int
val projectile_color : Graphics.color 