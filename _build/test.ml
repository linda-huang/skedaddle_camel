open OUnit2 
open Camel 
open Coin 
open Enemy
open Maze
open Position 
open Projectile

let camel_tests = [] 

let dist_test 
    (name : string) 
    ((x1, y1) : float * float)
    ((x2, y2) : float * float)
    (exp_dist : float) : test = 
  name >:: (fun _ -> 
      assert_equal exp_dist (dist (make_pos x1 y1) (make_pos x2 y2)) 
        ~printer:string_of_float)

let position_tests = [
  dist_test "origin" (0., 0.) (0., 0.) 0.;
  dist_test "3, 4, 5" (0., 3.) (4., 0.) 5.;
]

let maze_tests = []

let coin_tests = []

let enemy_tests = []

let suite = "test suite" >::: List.flatten [
    camel_tests;
    position_tests;
    maze_tests;
    coin_tests;
    enemy_tests;
  ]

let _ = run_test_tt_main suite 