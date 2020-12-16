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

(** [coin_arr pos] is a Coin.t array of coins with positions in [pos] *)
let coin_arr pos = 
  Array.map (fun (px, py) -> Coin.init (Position.init_pos (px, py)) 100) pos

(*******************************************************************
   tests from Caeml.ml
 *********************************************************************)

let camel_tests = [
  (* TODO *)
  (* Camel.init *)
  (* Camel.turn_right *)
  (* Camel.turn_left *)
  (* Camel.move_horiz *)
  (* Camel.adj_health *)
  (* Camel.adj_coin *)
  (* Camel.is_dead *)
] 
(*******************************************************************
   end tests from Camel.ml
 *********************************************************************)

(*******************************************************************
   tests from Coin.ml
 *********************************************************************)
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
      let exp_coin = Coin.init (Position.init_pos exp_val) 1 in  
      assert_equal exp_coin 
        (Coin.find_coin (Position.init_pos (cx, cy)) (coin_arr pos)) 
        ~printer:string_of_coin)

let coin_tests = []
(*******************************************************************
   end tests from Coin.ml
 *********************************************************************)

(*******************************************************************
   tests from Enemy.ml
 *********************************************************************)

let enemy_tests = []
(*******************************************************************
   end tests from Enemy.ml
 *********************************************************************)

(*******************************************************************
   tests from Genie.ml
 *********************************************************************)

let genie_tests = []
(*******************************************************************
   end tests from Genie.ml
 *********************************************************************)

(*******************************************************************
   tests from Hourglass.ml
 *********************************************************************)

let hourglass_tests = []
(*******************************************************************
   end tests from Hourglass.ml
 *********************************************************************)


(*******************************************************************
   tests from Potion.ml
 *********************************************************************)

let potion_tests = []
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
  (* pixel_to_tile *)
  pixel_to_tile_test "out of bounds" startpos (25, 25) Out_of_bounds;
  pixel_to_tile_test "corner origin (0, 0)" startpos startpos (Valid (0, 0));
  pixel_to_tile_test "edge origin (50, 0)" startpos 
    (startposx+3*Constant.tile_width, startposy-2*Constant.tile_width)
    (Valid (3, 2));
  pixel_to_tile_test "corner origin" startpos 
    (startposx+Constant.tile_width, startposy-Constant.tile_width) 
    (Valid (1,1));
]
(*******************************************************************
   end tests from Position.ml
 *********************************************************************)

(*******************************************************************
   tests from Projectile.ml
 *********************************************************************)

let projectile_tests = []
(*******************************************************************
   end tests from Projectile.ml
 *********************************************************************)


(*******************************************************************
   tests from Round_State.ml
 *********************************************************************)

(** [enemy_arr pos] is a Enemy.t array of enemies with positions in [pos] *)
let enemy_arr pos = 
  Array.map (fun (px, py) -> 
      Enemy.init (90 * Random.int 4) (Position.init_pos (px, py))) pos

(** [near_enemy_test name (cx, cy) (ex, ey) exp_val] constructs an OUnit
    test named [name] that asserts the quality of 
    [State.near_enemy] applied to a Camel at the position represented 
    by [(cx, cy)] and a State with enemy at position indicated by [(ex, ey)] 
    with [exp_val]. *)
let near_enemy_test 
    (name : string) 
    ((cx, cy) : int * int)
    (epos : (int * int) array)
    (exp_val : bool) : test = 
  name >:: (fun _ -> 
      let camel = Camel.init cx cy in                 
      let st = {camel = camel;
                maze = Maze.populate 100 100 (0,0);
                cols = 100; rows = 100;
                enemies = enemy_arr epos;
                coins = [||];
                potions = [||];
                projectiles = [];
                genie = None;
                hourglass = None;
                top_left_corner = startpos} in 
      assert_equal exp_val (near_enemy camel st)
        ~printer:string_of_bool)

(** [on_coin_test name (cx, cy) (x, y) exp_val] constructs an OUnit
    test named [name] that asserts the quality of 
    [Round_state.on_coin] applied to a Camel at the position represented 
    by [(cx, cy)] and a Round_state with a coin at position indicated by [(x, y)] 
    with [exp_val]. *)
let on_coin_test 
    (name : string) 
    ((cx, cy) : int * int)
    (pos : (int * int) array)
    (exp_val : bool) : test = 
  name >:: (fun _ -> 
      let camel = Camel.init cx cy in 
      let st = {camel = camel;
                maze = Maze.populate 100 100 (0,0);
                cols = 100; rows = 100;
                enemies = [||];
                coins = coin_arr pos;
                potions = [||];
                projectiles = [];
                genie = None;
                hourglass = None;
                top_left_corner = startpos} in 
      assert_equal exp_val (on_coin st) 
        ~printer:string_of_bool)

(** [rem_coin_test name (cx, cy) pos exp_exn] constructs an OUnit test named [name] 
    that asserts that [Round_state.find_coin] raises [exp_exn]. *)
(* let rem_coin_exn_test
    (name : string) 
    ((cx, cy) : int * int)
    (pos : (int * int) array)
    (exp_exn : exn) : test = 
   name >:: (fun _ -> 
      let camel = Camel.init cx cy in 
      let st = {camel = camel;
                maze = Maze.populate 100 100 (0,0);
                cols = 100; rows = 100;
                enemies = [||];
                coins = coin_arr pos;
                projectiles = [];
                top_left_corner = (43, 43)} in  
      assert_raises exp_exn (fun _ -> ((rem_coin (find_coin camel.pos st) st).coins ))) *)

(** [remove_coin_test name getcoin pos exp_val] constructs an OUnit
    test named [name] that asserts the quality of  
    [Round_state.remove_coin] applied to a coin at the position represented 
    by [getcoin] and a Round_state.t with coins at positions indicated by [pos] 
    against [exp_val]. *)
let remove_coin_test 
    (name : string) 
    (getcoin : (int * int))
    (pos : (int * int) array)
    (exp_val : (int * int) array) : test = 
  name >:: (fun _ -> 
      let st = {camel = Camel.init 0 0;
                maze = Maze.populate 100 100 (0,0);
                cols = 100; rows = 100;
                enemies = [||];
                coins = coin_arr pos;
                potions = [||];
                projectiles = [];
                genie = None;
                hourglass = None;
                top_left_corner = startpos} in 
      let exp_coin = coin_arr exp_val in  
      assert_equal ~cmp:cmp_set_like_arrs exp_coin 
        (Round_state.remove_coin 
           (Coin.init (Position.init_pos getcoin) 1) st).coins 
        ~printer:string_of_coinarr)


(* let proj_test name cx cy pos exp_val =
   name >:: (fun _ ->
   let camel = Camel.init cx cy in 
   let st = {camel = camel;
             maze = Maze.populate 100 100 (0,0);
             cols = 100; rows = 100;
             enemies = [||];
             coins = coin_arr pos;
             projectiles = []} in 
   shoot camel st |> 
   ) *)

let round_state_tests = [
  (* todo *)

  (* near_enemy *)
  near_enemy_test "same position, single enemy (5,5)" 
    (5, 5)[|(5, 5)|] true;
  near_enemy_test "multiple close enemies (5,5)" 
    (5, 5)[|(15, 15); (5, 5); (50, 50)|] true;
  near_enemy_test "single far enemy (50,50)" 
    (0, 0)[|(110, 110)|] false;
  near_enemy_test "multiple far enemies" 
    (0, 0)[|(150, 150); (250, 100); (90, 75)|] false;
  near_enemy_test "multiple enemies, single close" 
    (0, 0)[|(150, 150); (250, 100); (0, 4)|] true;
  (* on_coin *)
  on_coin_test "same position, single coin"
    (5, 5)[|(5, 5)|] true;
  on_coin_test "multiple coins, one close"
    (5, 5)[|(250, 100); (50, 55); (7, 5)|] true;
  on_coin_test "multiple coins, none on same tile"
    (5, 5)[|(250, 100); (50, 55); (50, 5)|] false;
  (* find_coin and rem_coin *)
  (* rem_coin_test "on only coin in maze" 
     (5, 5)[|(5, 5)|] [||];
     rem_coin_test "on one coin of multiple in maze" 
     (5, 5)[|(250, 100); (50, 55); (5, 5)|] 
     [|(250, 100); (50, 55);|];
     rem_coin_exn_test "not on any coins" 
     (25, 52)[|(250, 100); (150, 550); (115, 325)|] 
     (Invalid_argument "No coin here"); *)
]
(*******************************************************************
   end tests from Round_state.ml
 *********************************************************************)

let suite = "test suite" >::: List.flatten [
    camel_tests;
    coin_tests;
    enemy_tests;
    genie_tests;
    hourglass_tests;
    potion_tests;
    position_tests;
    projectile_tests;
    round_state_tests;
  ]

let _ = run_test_tt_main suite  