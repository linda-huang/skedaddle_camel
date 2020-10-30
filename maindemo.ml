open Graphics
open Camel
open Enemy
open Maze 
open State
open Scorer      

let window_width = 1000
let window_height = 2000

let fightingring = 
  [| [|Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall;|];
     [|Wall; Start; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Path; Wall;|];
     [|Wall; Path; Path; Path; Path; Path; Path; Path; Exit; Wall;|];
     [|Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall; Wall;|];
  |]

(** [draw_maze_elt x y color] draws a maze tile at [x] [y] and
    fills it with [color] *)
let draw_maze_elt x y color = 
  set_color color;
  fill_poly [|(x,y); (x+path_width,y); (x+path_width, y-path_width); 
              (x, y-path_width)|]

(** [draw_camel camel] draws a rectangle representing the player camel *)
let draw_camel (camel : Camel.t) = 
  let color = Graphics.rgb 220 206 192 in 
  set_color color; 
  let (x, y) = (int_of_float camel.pos.x, int_of_float camel.pos.y) in 
  fill_poly [|(x,y); (x+camel_width_int,y); 
              (x+camel_width_int, y-camel_width_int); 
              (x, y-camel_width_int)|]

(** [draw_enemy enemy] draws a rectangle representing an Enemy camel *)
let draw_enemy enemy = 
  let color = Graphics.rgb 179 27 27 in 
  set_color color; 
  let (x, y) = (int_of_float enemy.pos.x, int_of_float enemy.pos.y) in 
  fill_poly [|(x,y); (x+camel_width_int,y); 
              (x+camel_width_int, y-camel_width_int); 
              (x, y-camel_width_int)|]

(** [draw_walls gen_maze start_pos maze_row maze_col] draws [gen_maze] *)
let draw_walls gen_maze start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i*path_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + (j)*path_width , snd !curr_pos);
      if tile_type gen_maze j i = Wall then begin
        draw_maze_elt (fst !curr_pos) (snd !curr_pos) Graphics.black;
      end
      else
        draw_maze_elt (fst !curr_pos) (snd !curr_pos) Graphics.green;
    end
    done
  end
  done 

(** [draw_maze m n] generates a new maze dimensions [m] x [n]. 
    Requires: [m] and [n] to be positive and odd. *)
let draw_maze mz = 
  let maze_row = Array.length mz in
  let maze_col = Array.length mz.(0) in
  let window_height = maze_row * path_width + 200 in
  let window_width = maze_col * path_width + 200 in
  moveto (window_width / 2 - 55) (window_height - 50);
  Graphics.set_text_size 300; 
  let start_y = window_height (*- ((window_height- maze_row * path_width) / 2) *)in
  let start_x = 0 (* window_width - maze_col * path_width) / 2) *)in
  let start_pos = (start_x, start_y) in
  draw_walls mz start_pos maze_row maze_col

(** [draw_state st] is the Graphics representation of [st] *)
let draw_state st = 
  draw_maze st.maze; 
  draw_camel st.camel; 
  Array.iter draw_enemy st.enemies;
  let s = wait_next_event[Key_pressed] in
  if s.keypressed then Graphics.clear_graph ()

let game_over st scr = 
  let health = st.camel.health in 
  let coins = st.camel.coins in 
  (* let enem = st.camel.bodycount in *)
  Graphics.clear_graph ();
  Graphics.moveto 50 1000;
  Graphics.draw_string "Game ended!";
  Graphics.moveto 50 950;
  Graphics.draw_string ("Final health: " ^ string_of_int health);
  Graphics.moveto 50 900;
  (* Graphics.draw_string ("Enemies killed: " ^ string_of_int st.camel.bodycount);*)
  Graphics.moveto 50 850;
  Graphics.draw_string ("Coins collected: " ^ string_of_int coins);
  Graphics.moveto 50 800;
  Graphics.draw_string ("Final score: " ^ 
                        string_of_int (Scorer.score scr st.camel));
  let s = wait_next_event[Key_pressed] in
  if s.keypressed then exit 0 (*Graphics.close_graph ()*)

(** [is_dead camel] is if [camel] has run out of health *)
let is_dead camel = camel.health = 0

let at_exit (st : State.t) = 
  let camel = st.camel in 
  let (x, y) = State.curr_tile camel.pos in 
  Maze.tile_type st.maze x y = Exit 

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

(** [get_coin st] is [st] with the coin the camel is currently on removed *)
let get_coin (st : State.t) : State.t = 
  let c = find_coin st.camel.pos st in 
  rem_coin c st 

(** [update_state st] is [st] with all agents updated one move
    e.g. all enemies moved one step; projectiles moved one unit; 
    any applicable coins picked up; camel score and health adjusted *)
let update_state (st : State.t) : State.t = 
  let st' = st |> update_camel |> State.move_proj |> State.move_enemies in 
  if (State.on_coin st'.camel st') then get_coin st' else st'

(** [input st k] updates [st] in response to [k].
    It ends the game when [k] = '0' *)
let input (st : State.t) (k : char) : State.t =  
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

(** [run st] runs the game responding to key presses *)
let rec run (st : State.t) (scr : Scorer.t)= 
  Graphics.open_graph " ";
  Graphics.moveto 50 800; 
  Graphics.draw_string (string_of_float (Sys.time ()));
  Graphics.moveto 50 700;
  Graphics.draw_string "press a key to move (press 0 to exit)";
  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then 
    let newst = input st s.key in 
    ( (*Graphics.auto_synchronize false;*)
      Graphics.clear_graph ();
      draw_state newst;
      (* Graphics.auto_synchronize true; *)
      Graphics.set_color Graphics.black; 
      Graphics.moveto 50 750;
      Graphics.draw_string ("Began as: " ^ State.string_of_state st);
      Graphics.moveto 50 725;
      Graphics.draw_string ("Moved to: " ^ State.string_of_state newst);
      if at_exit newst then 
        (Graphics.clear_graph ();
         Graphics.moveto 50 550;
         Graphics.draw_string "welcome to a new maze!"; 
         let camel = Camel.init 0. 0. in 
         let st = State.init camel 10 10 5 in 
         let scr' = Scorer.update_time scr (Sys.time ()) in 
         run st scr')
      else run newst scr)

(** [init k] creates a new game State with camel initialized at the origin
    in a maze of dimensions 10x10 and then runs the game *)
let init k = 
  let camel = Camel.init 0. 0. in 
  let st = State.init camel 45 45 5 in 
  let scr = Scorer.init () in 
  draw_state st; 
  Graphics.moveto 20 700;
  Graphics.draw_string "check init";
  run st scr

(* Start on key press *)
let main () = 
  Graphics.open_graph " ";
  Graphics.set_window_title "Skedadle Camel";
  Graphics.resize_window window_width window_height;
  Graphics.set_text_size 300;
  Graphics.moveto 20 700;
  Graphics.draw_string "press a key to start";
  match Graphics.read_key () with 
  | k -> init k 

(* Execute the demo. *)
let () = main ()
