open Graphics
open Camel
open Enemy
open Maze 
open State      

let is_dead camel = camel.health = 0

(* [update_camel st] is the state with the camel's 
   health and coin total updated *)
let update_camel (st : State.t) : State.t = 
  let camel = st.camel in 
  let camel' = if (State.near_enemy camel st) then 
      {camel with health = camel.health - 1} else camel in 
  let camel'' = if (State.on_coin camel st) then 
      {camel with coins = camel.coins + 1} else camel' in 
  if (is_dead camel'') then failwith " game over " else 
    {st with camel = camel''}  

let draw_state state = failwith "todo" 

let input (st : State.t) (k : char) : State.t =  
  Graphics.clear_graph ();
  Graphics.moveto 50 500;
  let camel = st.camel in 
  let st' = 
    match k with 
    | '0' -> exit 0 
    | 'w' -> {st with camel = (Camel.move_vert camel 1.)}
    | 'a' -> {st with camel = (Camel.move_horiz camel ~-.1.)}
    | 's' -> {st with camel = (Camel.move_vert camel ~-.1.)}
    | 'd' -> {st with camel = (Camel.move_horiz camel 1.)}
    | 'e' -> {st with camel = (Camel.turn_right camel)}
    | 'q' -> {st with camel = (Camel.turn_left camel)}
    | ' ' -> State.shoot camel st
    | _ -> {st with camel = camel} 
  in 
  let st'' = if State.hit_wall st'.camel.pos st'.maze then st else st' in 
  st'' |> State.move_proj |> State.move_enemies 


let rec run (st : State.t) = 
  Graphics.moveto 50 500;
  Graphics.draw_string "press a key to move";
  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then 
    (Graphics.clear_graph ();
     let newst = input st s.key in 
     Graphics.moveto 50 400;
     Graphics.draw_string ("Began as: " ^ State.string_of_state st);
     Graphics.moveto 50 300;
     Graphics.draw_string ("Moved to: " ^ State.string_of_state newst);
     Graphics.moveto 50 200;
     run newst)

(*draw_state State.init_state; run State.init_state*)
let init k = 
  let camel = Camel.init 0. 0. in 
  let st = State.init camel 10 10 1 in 
  run st 

let main () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.resize_window 1000 2000;
  Graphics.set_text_size 300;
  Graphics.moveto 50 600;
  Graphics.draw_string "press a key to start";
  match Graphics.read_key () with 
  | k -> init k 

(* Execute the demo. *)
let () = main ()

(* Graphics.draw_image : image -> int -> int -> unit
   Draw the given image with lower left corner at the given point.*)