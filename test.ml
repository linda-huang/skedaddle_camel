(*  OUnit tested modules: Camel; Coin; Enemy; Genie; Potion; Position; 
    Projectile; Maze (some). 

    These tests were developed through glass box testing. We primarily test 
    functions that compute distances and conversion between pixel and tile 
    locations (Position.ml), testing whether a game object is in a maze 
    (find_potion, for example), and single-step movement of a game element 
    (like changing direction and moving one step). 

    Some testing of the maze generation was possible; primarily checking that 
    the position corresponding to upper-leftmost tile is a start tile and the 
    position corresponding to the lower-rightmost tile is an end tile. 
    The rest of the functionality in Maze depends on a DFS algorithm. 
    We analyzed the algorithm manually and determined it to be correct. 

    We omit testing functions such as initialization of the different game 
    elements, as these are combined with the use of Round_state.ml, 
    which we manually test through running the game. 

    The rest of the modules are manually tested: Draw; Hourglass; Timer; 
    Round_state; Game_state; Maze; Main. 
    Many of the functions our modules dealt with movement of game elements 
    that were time dependent or needed to respond to user keyboard input. 

    Additionally, these functions were not exposed, so we tested these manually 
    though observing how game elements interacted with each other 
    while running Main.ml. 

    Through this combination of OUnit testing, to ensure that backend 
    functionality worked as we expected, and manual testing via game play,
    to ensure that the graphical representation of the game elements 
    interacted smoothly and responded to user input, we believe that this 
    test suite demonstrates the correctness of the system.
*)

open OUnit2 
open Camel 
open Coin 
open Enemy
open Genie
open Hourglass 
open Maze
open Potion
open Position 
open Projectile
open Round_state 
open Random 

(*******************************************************************
   testing constants
   *********************************************************************)
(* pixel start positions corresponding to level 1
   r = 9, c = 13 *)
let startposx = 100
let startposy = 460
let startpos = startposx, startposy

(*******************************************************************
   helper functions
 *********************************************************************)
(** [string_of_tuple (x,y)] is the string representation of [(x,y)] *)
let string_of_tuple (x, y) = 
  "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")"

(** [string_of_tuple (x,y)] is the string representation of [(x,y)] *)
let string_of_tuple2 (x, y) = 
  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

(** [string_of_valid_tuple (x,y)] is [(x,y)] if it is not Out of Bounds *)
let string_of_valid_tuple = function 
  | Valid (x,y) ->  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
  | Out_of_bounds -> "OB"

(* from A2 *)
(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_arrs arr1 arr2 =
  let lst1 = Array.to_list arr1 in 
  let lst2 = Array.to_list arr2 in 
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [string_of_coinarr] is the string representation of an 
    array of coins *)
let string_of_coinarr (arr : Coin.t array) = 
  Array.to_list arr |> pp_list (fun (x : Coin.t) -> string_of_pos x.pos)

(*******************************************************************
   tests from Camel.ml
 *********************************************************************)
(** [camel_turn_right_test name camel_pos num_turns 
    exp_camel_pos exp_camel_dir] constructs an OUnit test named [name] 
    that asserts the quality of [Camel.turn_right] applied to a camel with 
    position [camel_pos] and dir 0 turned right [num_turns] times 
    with [exp_camel_pos] and [exp_camel_dir] *)
let camel_turn_right_test  
    (name : string) 
    (camel_pos_x, camel_pos_y : int * int)
    (num_turns : int)
    (exp_camel_pos : int * int)
    (exp_camel_dir : int) : test = 
  name >:: (fun _ -> 
      let rec rep_turns i camel = 
        if i > 0 then rep_turns (i - 1) (Camel.turn_right camel)
        else camel in 
      let newcamel = rep_turns num_turns 
          (Camel.init camel_pos_x camel_pos_y) in 
      assert_equal (Position.init_pos exp_camel_pos)
        newcamel.pos ~printer:string_of_pos;
      assert_equal exp_camel_dir
        newcamel.dir ~printer:string_of_int)

(** [camel_turn_left_test name camel_pos num_turns 
    exp_camel_pos exp_camel_dir] constructs an OUnit test named [name] 
    that asserts the quality of [Camel.turn_left] applied to a camel with 
    position [camel_pos] and dir 0 turned left [num_turns] times 
    with [exp_camel_pos] and [exp_camel_dir] *)
let camel_turn_left_test  
    (name : string) 
    (camel_pos_x, camel_pos_y : int * int)
    (num_turns : int)
    (exp_camel_pos : int * int)
    (exp_camel_dir : int) : test = 
  name >:: (fun _ -> 
      let rec rep_turns i camel = 
        if i > 0 then rep_turns (i - 1) (Camel.turn_left camel)
        else camel in 
      let newcamel = rep_turns num_turns 
          (Camel.init camel_pos_x camel_pos_y) in 
      assert_equal (Position.init_pos exp_camel_pos)
        newcamel.pos ~printer:string_of_pos;
      assert_equal exp_camel_dir
        newcamel.dir ~printer:string_of_int)

(** [camel_move_horiz_test name camel_pos dir key 
    exp_camel_pos exp_camel_dir] constructs an OUnit test named [name] 
    that asserts the quality of [Camel.move_horiz] applied to a camel with 
    position [camel_pos] towards direction corresponding to [dir] and [key]
    with [exp_camel_pos] and [exp_camel_dir] *)
let camel_move_horiz_test  
    (name : string) 
    (camel_pos_x, camel_pos_y : int * int)
    (pos : int)
    (key : char)
    (exp_camel_pos : int * int)
    (exp_camel_dir : int) : test = 
  name >:: (fun _ -> 
      let newcamel = Camel.move_horiz 
          (Camel.init camel_pos_x camel_pos_y) pos key in 
      assert_equal (Position.init_pos exp_camel_pos)
        newcamel.pos ~printer:string_of_pos;
      assert_equal exp_camel_dir
        newcamel.dir ~printer:string_of_int)

(** [camel_move_vert_test name camel_pos dir key 
    exp_camel_pos exp_camel_dir] constructs an OUnit test named [name] 
    that asserts the quality of [Camel.move_vert] applied to a camel with 
    position [camel_pos] towards direction corresponding to [dir] and [key]
    with [exp_camel_pos] and [exp_camel_dir] *)
let camel_move_vert_test  
    (name : string) 
    (camel_pos_x, camel_pos_y : int * int)
    (pos : int)
    (key : char)
    (exp_camel_pos : int * int)
    (exp_camel_dir : int) : test = 
  name >:: (fun _ -> 
      let newcamel = Camel.move_vert 
          (Camel.init camel_pos_x camel_pos_y) pos key in 
      assert_equal (Position.init_pos exp_camel_pos)
        newcamel.pos ~printer:string_of_pos;
      assert_equal exp_camel_dir
        newcamel.dir ~printer:string_of_int)

let camel_tests = [
  camel_turn_right_test "turn R 1" startpos 1 startpos ~-90;
  camel_turn_right_test "turn R 2" startpos 2 startpos ~-180;
  camel_turn_right_test "turn R 3" startpos 3 startpos ~-270;
  camel_turn_right_test "turn R 4" startpos 4 startpos 0;
  camel_turn_right_test "turn R 5" startpos 5 startpos ~-90;
  camel_turn_left_test "turn L 1" startpos 1 startpos 90;
  camel_turn_left_test "turn L 2" startpos 2 startpos 180;
  camel_turn_left_test "turn L 3" startpos 3 startpos 270;
  camel_turn_left_test "turn L 4" startpos 4 startpos 0;
  camel_turn_left_test "turn L 5" startpos 5 startpos 90;
  camel_move_horiz_test "move right d" startpos 1 'd' 
    (startposx+Constant.camel_speed, startposy) 0;
  camel_move_horiz_test "move left a" (300, 300) ~-1 'a'
    (300-Constant.camel_speed, 300) 180;
  camel_move_vert_test "move up w" (300, 300) 1 'w'
    (300, 300+Constant.camel_speed) 90;
  camel_move_vert_test "move up s" (300, 300) ~-1 's'
    (300, 300-Constant.camel_speed) 270;
] 
(*******************************************************************
   end tests from Camel.ml
 *********************************************************************)

(*******************************************************************
   tests from Coin.ml
 *********************************************************************)
(** [coin_arr pos] is a Coin.t array of coins with positions in [pos] *)
let coin_arr pos = 
  Array.map (fun (px, py) -> Coin.init (Position.init_pos (px, py)) 10) pos

(** [find_coin_test name (cx, cy) pos exp_val] constructs an OUnit
    test named [name] that asserts the quality of 
    [Coin.find_coin] applied to the position corresponding to [(cx, cy)] and 
    Coin.t array with positions corresponding to [pos] 
    against [exp_val] *)
let find_coin_test 
    (name : string) 
    ((cx, cy) : int * int)
    (pos : (int * int) array)
    (exp_val : (int * int)) : test = 
  name >:: (fun _ -> 
      let exp_coin = Coin.init (Position.init_pos exp_val) 10 in  
      assert_equal exp_coin 
        (Coin.find_coin (Position.init_pos (cx, cy)) (coin_arr pos)) 
        ~printer:string_of_coin)

(** [find_coin_exn_test name curr_pos pos_arr exp_exn] constructs 
    an OUnit test named [name] that asserts that [coin.find_coin] 
    raises [exp_exn]. *)
let find_coin_exn_test
    (name : string) 
    (curr_pos : int * int)
    (pos_arr : (int * int) array)
    (exp_exn : exn) : test = 
  name >:: (fun _ -> 
      let coins = coin_arr pos_arr in  
      assert_raises exp_exn (fun _ -> 
          (Coin.find_coin (Position.init_pos curr_pos) coins)))

let coin_tests = [
  find_coin_test "first coin" (300, 325) 
    [|(300, 322); (200, 425); (1000, 60)|] (300, 322);
  find_coin_test "middle coin" (1000, 525) 
    [|(300, 322); (1000, 527); (200, 425)|] (1000, 527);
  find_coin_test "last coin" (1000, 525) 
    [|(300, 322); (200, 425); (1000, 527)|] (1000, 527);
  find_coin_test "long coin list, middle" (1000, 525)
    [|(300, 322); (400, 400); (1000, 527); (500, 400);
      (400, 600); (700, 480); (200, 425)|] (1000, 527);
  find_coin_exn_test "no such coin" (1000, 525) 
    [|(300, 322); (200, 425)|] (Invalid_argument "No coin here");
]
(*******************************************************************
   end tests from Coin.ml
 *********************************************************************)

(*******************************************************************
   tests from Enemy.ml
 *********************************************************************)
(** [enemy_move_test name enemy_pos enemy_dir exp_enemy_pos exp_enemy_dir] 
    constructs an OUnit test named [name] that asserts the quality of 
    [enemy.move] applied to a enemy with direction [enemy_dir] and [enemy_pos] 
    with [exp_enemy_pos] and [exp_enemy_dir]. *)
let enemy_move_test  
    (name : string) 
    (enemy_pos : int * int)
    (enemy_dir : int)
    (exp_enemy_pos : int * int)
    (exp_enemy_dir : int) : test = 
  name >:: (fun _ -> 
      let newenemy = Enemy.move 
          (Enemy.init enemy_dir (Position.init_pos enemy_pos)) in 
      assert_equal (Position.init_pos exp_enemy_pos) 
        newenemy.pos ~printer:string_of_pos;
      assert_equal exp_enemy_dir
        newenemy.dir ~printer:string_of_int)

(** [enemy_change_dir_test name enemy_pos enemy_dir turn 
    exp_enemy_pos exp_enemy_dir] constructs an OUnit test named [name] 
    that asserts the quality of [enemy.change_dir] applied to a enemy with 
    direction [enemy_dir] and position [enemy_pos] to direction [turn] 
    with [exp_enemy_pos] and [exp_enemy_dir] *)
let enemy_change_dir_test  
    (name : string) 
    (enemy_pos : int * int)
    (enemy_dir : int)
    (turn : int) 
    (exp_enemy_pos : int * int)
    (exp_enemy_dir : int) : test = 
  name >:: (fun _ -> 
      let newenemy = Enemy.change_dir 
          (Enemy.init enemy_dir (Position.init_pos enemy_pos)) turn in 
      assert_equal (Position.init_pos exp_enemy_pos)
        newenemy.pos ~printer:string_of_pos;
      assert_equal exp_enemy_dir
        newenemy.dir ~printer:string_of_int)

let enemy_tests = [
  enemy_move_test "move dir 0" (300, 300) 0 
    (300 + Constant.enemy_speed, 300) 0;
  enemy_move_test "move dir 90" (250, 250) 90 
    (250, 250 + Constant.enemy_speed) 90;
  enemy_move_test "move dir 180" (275, 300) 180
    (275 - Constant.enemy_speed, 300) 180;
  enemy_move_test "move dir 270" (275, 275) 270
    (275, 275 - Constant.enemy_speed) 270;
  enemy_change_dir_test "turn 0 -> 180" startpos 0 180 startpos 180;
  enemy_change_dir_test "turn 90 -> 180" (300, 300) 90 180 (300, 300) 180;
  enemy_change_dir_test "turn 0 -> 270" (275, 275) 0 270 (275, 275) 270;
]
(*******************************************************************
   end tests from Enemy.ml
 *********************************************************************)

(*******************************************************************
   tests from Genie.ml
 *********************************************************************)
(** [genie_move_test name genie_pos genie_dir exp_genie_pos exp_genie_dir] 
    constructs an OUnit test named [name] that asserts the quality of 
    [Genie.move] applied to a genie with direction [genie_dir] and [genie_pos] 
    with [exp_genie_pos] and [exp_genie_dir]. *)
let genie_move_test  
    (name : string) 
    (genie_pos : int * int)
    (genie_dir : int)
    (exp_genie_pos : int * int)
    (exp_genie_dir : int) : test = 
  name >:: (fun _ -> 
      let newgenie = Genie.move 
          (Genie.init genie_dir (Position.init_pos genie_pos)) in 
      assert_equal (Position.init_pos exp_genie_pos) 
        newgenie.pos ~printer:string_of_pos;
      assert_equal exp_genie_dir
        newgenie.dir ~printer:string_of_int)

(** [genie_change_dir_test name genie_pos genie_dir turn 
    exp_genie_pos exp_genie_dir] constructs an OUnit test named [name] 
    that asserts the quality of [Genie.change_dir] applied to a genie with 
    direction [genie_dir] and position [genie_pos] to direction [turn] 
    with [exp_genie_pos] and [exp_genie_dir] *)
let genie_change_dir_test  
    (name : string) 
    (genie_pos : int * int)
    (genie_dir : int)
    (turn : int) 
    (exp_genie_pos : int * int)
    (exp_genie_dir : int) : test = 
  name >:: (fun _ -> 
      let newgenie = Genie.change_dir 
          (Genie.init genie_dir (Position.init_pos genie_pos)) turn in 
      assert_equal (Position.init_pos exp_genie_pos)
        newgenie.pos ~printer:string_of_pos;
      assert_equal exp_genie_dir
        newgenie.dir ~printer:string_of_int)

let genie_tests = [
  genie_move_test "move dir 0" (300, 300) 0 
    (300+Constant.genie_speed, 300) 0;
  genie_move_test "move dir 90" (250, 250) 90 
    (250, 250+Constant.genie_speed) 90;
  genie_move_test "move dir 180" (275, 300) 180
    (275-Constant.genie_speed, 300) 180;
  genie_move_test "move dir 270" (275, 275) 270
    (275, 275-Constant.genie_speed) 270;
  genie_change_dir_test "turn 0 -> 180" startpos 0 180 startpos 180;
  genie_change_dir_test "turn 90 -> 180" (300, 300) 90 180 (300, 300) 180;
  genie_change_dir_test "turn 0 -> 270" (275, 275) 0 270 (275, 275) 270;
]
(*******************************************************************
   end tests from Genie.ml
 *********************************************************************)

(*******************************************************************
   tests from Maze.ml
 *********************************************************************)
(** [string_of_tile_type tile] is the string representation of [tile] *)
let string_of_tile_type tile = 
  match tile with 
  | Wall _ -> "wall"
  | Path -> "path"
  | Exit -> "exit"
  | Start -> "start"
  | Power_Path _ -> "power"

(** [tile_type_test name r c tilex tiley exp_type] constructs 
    an OUnit test named [name] that asserts the quality of 
    [Maze.tile_type] applied to a maze with [r] rows and [c] columns 
    and the tile at position [tilex] [tiley] with [exp_type] *)
let tile_type_test  
    (name : string) 
    (r : int)
    (c : int)
    (tiler : int)
    (tilec : int)
    (exp_type : Maze.t) : test = 
  name >:: (fun _ -> 
      let maze = Maze.populate c r (0,0) in 
      assert_equal exp_type (tile_type maze tilec tiler) 
        ~printer:string_of_tile_type)

let maze_tests = [
  tile_type_test "start" 11 13 0 0 Start;
  tile_type_test "exit" 11 13 10 12 Exit 
]
(*******************************************************************
   end tests from Maze.ml
 *********************************************************************)

(*******************************************************************
   tests from Potion.ml
 *********************************************************************)
(** [potion_arr pos] is a Potion.potion array of potions 
    with positions in [pos] *)
let potion_arr pos = 
  Array.map (fun (px, py) -> 
      Potion.init (Position.init_pos (px, py))) pos

(** [find_potion_test name curr_pos pos_arr exp_potion_pos] constructs 
    an OUnit test named [name] that asserts the quality of [find_potion] when 
    applied to the position corresponding to [curr_pos] and the array of potions
    with positions corresponding to those found in [pos_arr] 
    to [exp_potion_pos] *)
let find_potion_test 
    (name : string) 
    (curr_pos : int * int)
    (pos_arr : (int * int) array)
    (exp_potion_pos : int * int) : test = 
  name >:: (fun _ -> 
      let potions = potion_arr pos_arr in  
      assert_equal (Position.init_pos exp_potion_pos)
        (Potion.find_potion (Position.init_pos curr_pos) potions).pos 
        ~printer:string_of_pos)

(** [find_potion_exn_test name curr_pos pos_arr exp_exn] constructs 
    an OUnit test named [name] that asserts that [Potion.find_potion] 
    raises [exp_exn]. *)
let find_potion_exn_test
    (name : string) 
    (curr_pos : int * int)
    (pos_arr : (int * int) array)
    (exp_exn : exn) : test = 
  name >:: (fun _ -> 
      let potions = potion_arr pos_arr in  
      assert_raises exp_exn (fun _ -> 
          (Potion.find_potion (Position.init_pos curr_pos) potions)))

let potion_tests = [
  find_potion_test "first potion" (300, 325) 
    [|(300, 322); (200, 425); (1000, 60)|] (300, 322);
  find_potion_test "middle potion" (1000, 525) 
    [|(300, 322); (1000, 527); (200, 425)|] (1000, 527);
  find_potion_test "last potion" (1000, 525) 
    [|(300, 322); (200, 425); (1000, 527)|] (1000, 527);
  find_potion_exn_test "no such potion" (1000, 525) 
    [|(300, 322); (200, 425)|] (Invalid_argument "No potion here");
]
(*******************************************************************
   end tests from Potion.ml
 *********************************************************************)

(*******************************************************************
   tests from Position.ml
 *********************************************************************)
(** [dist_test name d1 d2 exp_dist] constructs an OUnit
    test named [name] that asserts the quality of 
    [Position.dist] applied to the positions represented by [p1] and [p2]
    with [exp_dist]. *)
let dist_test 
    (name : string) 
    ((x1, y1) : int * int)
    ((x2, y2) : int * int)
    (exp_dist : int) : test = 
  name >:: (fun _ -> 
      assert_equal exp_dist 
        (Position.dist (init_pos (x1, y1)) (init_pos (x2, y2))) 
        ~printer:string_of_int)

(** [tile_to_pixel_test name startpos (x1, x2) exp_pix] constructs an OUnit 
    test named [name] that asserts the quality of 
    [Position.tile_to_pixel startpos (x1, x1)] with [exp_pix]. *)
let tile_to_pixel_test
    (name : string)
    (startpos : int * int)
    ((x1, y1) : int * int)
    (exp_pix : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal exp_pix
        (Position.tile_to_pixel startpos (x1, y1)) 
        ~printer:string_of_tuple2)

(** [pixel_to_tile_test name startpos (x1, x2) exp_tile] constructs an OUnit 
    test named [name] that asserts the quality of 
    [Position.pixel_to_tile (x1, x1) startpos] with [exp_tile]. *)
let pixel_to_tile_test
    (name : string)
    (startpos : int * int)
    ((x1, y1) : int * int)
    (exp_tile : Position.v) : test = 
  name >:: (fun _ -> 
      assert_equal exp_tile
        (Position.pixel_to_tile (init_pos (x1, y1)) startpos) 
        ~printer:string_of_valid_tuple)

let position_tests = [
  dist_test "origin" (0, 0) (0, 0) 0;
  dist_test "3, 4, 5" (0, 3) (4, 0) 5;
  (* tile_to_pixel *)
  tile_to_pixel_test "origin" startpos (0, 0) (120, 440);
  tile_to_pixel_test "(1,1)" startpos (1, 1) (160, 400);
  tile_to_pixel_test "(0, 10)" startpos (0, 10) (120, 40);
  tile_to_pixel_test "(15, 10)" startpos (15, 9) (720, 80);
  (* pixel_to_tile *)
  pixel_to_tile_test "out of bounds" startpos (25, 25) Out_of_bounds;
  pixel_to_tile_test "corner origin (0, 0)" startpos startpos (Valid (0, 0));
  pixel_to_tile_test "within origin" startpos (startposx + 10, startposy - 10)
    (Valid (0, 0));
  pixel_to_tile_test "top edge origin" startpos (startposx + 20, startposy)
    (Valid (0, 0));
  pixel_to_tile_test "left edge origin" startpos 
    (startposx, startposy - Constant.tile_radius) (Valid (0, 0));
  pixel_to_tile_test "bottom edge origin" startpos 
    (startposx + Constant.tile_radius, startposy - Constant.tile_width + 1) 
    (Valid (0, 0));
  pixel_to_tile_test "right edge origin" startpos 
    (startposx + Constant.tile_width - 1, startposy - Constant.tile_radius) 
    (Valid (0, 0));
  pixel_to_tile_test "edge (3, 2)" startpos 
    (startposx + 3 * Constant.tile_width, startposy - 2 * Constant.tile_width)
    (Valid (3, 2));
  pixel_to_tile_test "corner (1,1)" startpos 
    (startposx + Constant.tile_width, startposy - Constant.tile_width) 
    (Valid (1,1));
]
(*******************************************************************
   end tests from Position.ml
 *********************************************************************)

(*******************************************************************
   tests from Projectile.ml
 *********************************************************************)
(** [projectile_move_test name projectile_pos projectile_dir 
    exp_projectile_pos exp_projectile_dir] 
    constructs an OUnit test named [name] that asserts the quality of 
    [Projectile.move] applied to a projectile with direction [projectile_dir] 
    and position [projectile_pos] 
    with [exp_projectile_pos] and [exp_projectile_dir]. *)
let projectile_move_test  
    (name : string) 
    (projectile_pos : int * int)
    (projectile_dir : int)
    (exp_projectile_pos : int * int)
    (exp_projectile_dir : int) : test = 
  name >:: (fun _ -> 
      let newprojectile = Projectile.move_proj 
          (Projectile.init projectile_dir (Position.init_pos projectile_pos)) in 
      assert_equal (Position.init_pos exp_projectile_pos) 
        newprojectile.pos ~printer:string_of_pos;
      assert_equal exp_projectile_dir
        newprojectile.dir ~printer:string_of_int)

let projectile_tests = [
  projectile_move_test "move dir 0" (300, 300) 0 
    (300 + Constant.projectile_speed, 300) 0;
  projectile_move_test "move dir 90" (250, 250) 90 
    (250, 250 + Constant.projectile_speed) 90;
  projectile_move_test "move dir 180" (275, 300) 180
    (275 - Constant.projectile_speed, 300) 180;
  projectile_move_test "move dir 270" (275, 275) 270
    (275, 275 - Constant.projectile_speed) 270;
]
(*******************************************************************
   end tests from Projectile.ml
 *********************************************************************)

(*******************************************************************
   end tests from Round_state.ml
 *********************************************************************)

let suite = "test suite" >::: List.flatten [
    camel_tests;
    coin_tests;
    enemy_tests;
    genie_tests;
    maze_tests;
    potion_tests;
    position_tests;
    projectile_tests;
  ]

let _ = run_test_tt_main suite  