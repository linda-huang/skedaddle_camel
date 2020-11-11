open Graphics
open Camel
open Enemy
open Maze 
open Round_state
open Scorer
open Draw
open Game_state

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
  else st 

(** [input st k] updates [st] in response to [k].
    It ends the game when [k] = '0' *)
let input (gs : Game_state.game_state) : Game_state.game_state = 
  (* while not key pressed
     if Unix.time mod 1000 (some small time increment?) then move all proj and enemies *)
  let rec wait_kp (gs : Game_state.game_state) : Game_state.game_state = 
    if not (Graphics.key_pressed ()) then  
      let st' = update_round_state gs.round_state gs.score in
      let gs' =  if Camel.is_dead st'.camel 
        then Game_state.set_game_state gs GameOver 
        else {gs with round_state = st'} in 
      Draw.draw_game_state gs';
      (* Graphics.synchronize (); *)
      wait_kp gs' 
    else gs
  in 
  let gs = wait_kp gs in  
  let camel = gs.round_state.camel in 
  let k = Graphics.read_key () in 
  let st = gs.round_state in 
  let st' = 
    match k with 
    | '0' -> exit 0  
    | 'w' -> {st with camel = (Camel.move_vert camel 1 'w')}
    | 'a' -> {st with camel = (Camel.move_horiz camel ~-1 'a')}
    | 's' -> {st with camel = (Camel.move_vert camel ~-1 's')}
    | 'd' -> {st with camel = (Camel.move_horiz camel 1 'd')}
    | 'e' -> {st with camel = (Camel.turn_right camel)}
    | 'q' -> {st with camel = (Camel.turn_left camel)}
    | ' ' -> shoot camel st
    | _ -> {st with camel = camel} 
  in
  let st'' = if Round_state.hit_wall st' st'.camel.pos st'.camel.dir
    then st else st' in 
  let finst = st'' |> move_proj |> move_enemies |> hit_enemy in 
  {gs with round_state = finst}

(** [run st] runs game responding to key presses *)
let rec run (gs : Game_state.game_state) = 
  Graphics.moveto 50 800; 
  Graphics.draw_string (string_of_float (Sys.time ()));
  Graphics.moveto 50 700;
  (* Graphics.draw_string "press a key to move (press 0 to exit)"; *)
  let newgs = input gs in 
  Draw.draw_game_state newgs;
  let coord_mapping = Position.pixel_to_tile gs.round_state.camel.pos 
      gs.round_state.top_left_corner in
  match coord_mapping with 
  | Position.Out_of_bounds -> Graphics.draw_string "out of bounds";
  | Valid (col, row) -> 
    Graphics.moveto 0 0;
    Graphics.set_text_size 50;
    Graphics.set_color Graphics.white;
    Graphics.fill_poly [|(0,0);
                         (0,40);
                         (300,40);
                         (300,0)|];
    Graphics.set_color Graphics.red;
    Graphics.draw_string "current tile ";
    Graphics.draw_string (string_of_int col);
    Graphics.draw_string " ";
    Graphics.draw_string (string_of_int row);
    Graphics.draw_string " ";
    Graphics.draw_string "current direction ";
    Graphics.draw_string (string_of_int (newgs.round_state.camel.dir));
    let extract_wall_type maze col row = 
      match Maze.tile_type maze col row with
      | Wall -> "wall"
      | Path -> "path"
      | Exit -> "exit"
      | Start -> "start" in
    Graphics.draw_string (extract_wall_type gs.round_state.maze col row);
    Graphics.set_color Graphics.black;
    if extract_wall_type gs.round_state.maze col row = "exit" then 
      run (Game_state.new_level gs)  
    else run newgs

(** [init k] creates a new game round_state with camel initialized at the origin
    in a maze of dimensions 10x10 and then runs the game *)
let init () = 
  let st = Round_state.init 21 21 5 in 
  let gs = Game_state.init st in 
  draw_game_state gs; 
  Graphics.moveto 20 700;
  Graphics.synchronize ();
  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then 
    let gs' = Game_state.new_level gs in 
    (* Graphics.draw_string "check init"; *)
    run gs'

(* Start on key press *)
let main () = 
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  Graphics.set_window_title "Skedaddle Camel";
  Graphics.set_text_size 300;
  Graphics.moveto 20 700;
  Graphics.draw_string "press any key to start";
  Graphics.synchronize ();

  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then init ()

(* Execute the demo. *)
let () = main ()
