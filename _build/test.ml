open OUnit2 
open Camel 
open Coin 
open Enemy
open Maze
open Position 
open Projectile
open State 
open Random 

(*******************************************************************
   tests from Position.ml
 *********************************************************************)
(** [dist_test name d1 d2 exp_dist] constructs an OUnit
    test named [name] that asserts the quality of 
    [Position.dist] applied to the positions represented by [p1] and [p2]
    with [exp_dist]. *)
let dist_test 
    (name : string) 
    ((x1, y1) : float * float)
    ((x2, y2) : float * float)
    (exp_dist : float) : test = 
  name >:: (fun _ -> 
      assert_equal exp_dist (Position.dist (make_pos x1 y1) (make_pos x2 y2)) 
        ~printer:string_of_float)

let position_tests = [
  dist_test "origin" (0., 0.) (0., 0.) 0.;
  dist_test "3, 4, 5" (0., 3.) (4., 0.) 5.;
]
(*******************************************************************
   end tests from Position.ml
 *********************************************************************)

let maze_tests = []

let coin_tests = []

let camel_tests = [] 

(*******************************************************************
   tests from Enemy.ml
 *********************************************************************)

let enemy_tests = []
(*******************************************************************
   end tests from Enemy.ml
 *********************************************************************)

(*******************************************************************
   tests from State.ml
 *********************************************************************)

(** [enemy_arr pos] is a Enemy.t array of enemies with positions in [pos] *)
let enemy_arr pos = 
  Array.map (fun (px, py) -> 
      Enemy.init (90 * Random.int 4) (Position.make_pos px py)) pos

(** [string_of_tuple (x,y)] is the string representation of [(x,y)] *)
let string_of_tuple (x, y) = 
  "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")"

(** [tile_to_pixel_test name x y exp_loc] constructs an OUnit test named [name] 
    that asserts the quality of [State.tile_to_pixel x y] with [exp_val]. *)
let tile_to_pixel_test 
    (name : string) 
    (x : int) 
    (y : int)
    (exp_loc : (float * float)) : test = 
  name >:: (fun _ -> assert_equal (tile_to_pixel x y) exp_loc 
               ~printer:string_of_tuple)

(** [near_enemy_test name (cx, cy) (ex, ey) exp_val] constructs an OUnit
    test named [name] that asserts the quality of 
    [State.near_enemy] applied to a Camel at the position represented 
    by [(cx, cy)] and a State with enemy at position indicated by [(ex, ey)] 
    with [exp_val]. *)
let near_enemy_test 
    (name : string) 
    ((cx, cy) : float * float)
    (epos : (float * float) array)
    (exp_val : bool) : test = 
  name >:: (fun _ -> 
      let camel = Camel.init cx cy in 
      let st = {camel = camel;
                maze = Maze.populate 100 100 (0,0);
                x_size = 100; y_size = 100;
                enemies = enemy_arr epos;
                coins = [||];
                projectiles = []} in 
      assert_equal exp_val (near_enemy camel st)
        ~printer:string_of_bool)

(** [coin_arr pos] is a Coin.t array of coins with positions in [pos] *)
let coin_arr pos = 
  Array.map (fun (px, py) -> Coin.init (Position.make_pos px py) 100) pos

(** [on_coin_test name (cx, cy) (x, y) exp_val] constructs an OUnit
    test named [name] that asserts the quality of 
    [State.on_coin] applied to a Camel at the position represented 
    by [(cx, cy)] and a State with a coin at position indicated by [(x, y)] 
    with [exp_val]. *)
let on_coin_test 
    (name : string) 
    ((cx, cy) : float * float)
    (pos : (float * float) array)
    (exp_val : bool) : test = 
  name >:: (fun _ -> 
      let camel = Camel.init cx cy in 
      let st = {camel = camel;
                maze = Maze.populate 100 100 (0,0);
                x_size = 100; y_size = 100;
                enemies = [||];
                coins = coin_arr pos;
                projectiles = []} in 
      assert_equal exp_val (on_coin camel st) 
        ~printer:string_of_bool)

let state_tests = [
  (* todo *)
  (* tile_to_pixel *)
  tile_to_pixel_test "origin" 0 0 (25., 25.);
  tile_to_pixel_test "(1,1)" 1 1 (75., 75.);
  tile_to_pixel_test "(0, 100)" 0 100 (25., 5025.);
  (* near_enemy *)
  near_enemy_test "same position, single enemy (5.,5.)" 
    (5., 5.)[|(5., 5.)|] true;
  near_enemy_test "multiple close enemies (5.,5.)" 
    (5., 5.)[|(15., 15.); (5., 5.); (50., 50.)|] true;
  near_enemy_test "single far enemy (50.,50.)" 
    (0., 0.)[|(110., 110.)|] false;
  near_enemy_test "multiple far enemies" 
    (0., 0.)[|(150., 150.); (250., 100.); (90., 75.)|] false;
  near_enemy_test "multiple enemies, single close" 
    (0., 0.)[|(150., 150.); (250., 100.); (50., 55.)|] true;
  (* on_coin *)
  on_coin_test "same position, single coin"
    (5., 5.)[|(5., 5.)|] true;
  on_coin_test "multiple coins, one close"
    (5., 5.)[|(250., 100.); (50., 55.); (7., 5.)|] true;
  on_coin_test "multiple coins, none on same tile"
    (5., 5.)[|(250., 100.); (50., 55.); (50., 5.)|] false;
]
(*******************************************************************
   end tests from State.ml
 *********************************************************************)

let suite = "test suite" >::: List.flatten [
    camel_tests;
    position_tests;
    maze_tests;
    coin_tests;
    enemy_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite 