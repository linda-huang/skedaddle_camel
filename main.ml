open Graphics
open Camel
open Enemy
open Maze 
open Round_state
open Scorer
open Draw
open Game_state
open Unix
open Timer 

let input (gs : Game_state.game_state) (timer : Timer.timer) : Game_state.game_state = 
  let rec wait_kp (gs : Game_state.game_state) (timer : Timer.timer) : Game_state.game_state = 
    Unix.sleepf 0.001;
    if not (Graphics.key_pressed ()) then  
      let timer = Timer.update_timer timer in 
      let gs' = Game_state.update_game_state gs timer in 
      Draw.draw_game_state gs' timer;
      wait_kp gs' timer 
    else gs
  in 
  let gs = wait_kp gs timer in  
  let camel = gs.round_state.camel in 
  let st = gs.round_state in 
  let st' = 
    match Graphics.read_key () with 
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
  let finst = st'' |> update_round_state in 
  {gs with round_state = finst}

(** [flush_keypress ()] clears the queue of key presses 
    from [Graphics.read_key ()] *)
let rec flush_keypress () = 
  if Graphics.key_pressed () 
  then (ignore (read_key ()); flush_keypress ();)
  else () 

let rec run (gs : Game_state.game_state) (timer : Timer.timer) = 
  Graphics.moveto 50 10;
  let newgs = input gs timer in 
  let timer = Timer.update_timer timer in 
  Draw.draw_game_state newgs timer;
  let coord_mapping = Position.pixel_to_tile gs.round_state.camel.pos 
      gs.round_state.top_left_corner in
  match coord_mapping with 
  | Position.Out_of_bounds -> Graphics.draw_string "out of bounds";
  | Valid (col, row) -> 
    let extract_wall_type maze col row = 
      match Maze.tile_type maze col row with
      | Wall -> "wall"
      | Path -> "path"
      | Exit -> "exit"
      | Start -> "start" in
    Graphics.set_color Graphics.black;
    if extract_wall_type gs.round_state.maze col row = "exit" 
    then begin 
      flush_keypress (); 
      (* draw transition *)
      let transition_gs = Game_state.new_level gs in 
      Draw.draw_game_state transition_gs timer; 
      (* wait until use presses [x] to go to next level *)
      let rec go_to_nextlvl () = 
        match Graphics.read_key () with 
        | 'x' -> begin 
            let levelup_gs = Game_state.new_level transition_gs in 
            let timer = Timer.init_timer () in 
            Draw.draw_game_state levelup_gs timer; 
            Unix.sleep 1;
            match Graphics.read_key () with 
            | _ -> let timer = Timer.init_timer () in  
              run levelup_gs timer
          end 
        | _ -> go_to_nextlvl () in 
      go_to_nextlvl ()
    end 
    else 
      let timer = Timer.update_timer timer in 
      run newgs timer 

let init () = 
  let st = Round_state.init 21 21 5 1 in 
  let gs = Game_state.init st in 
  let timer = Timer.init_timer () in 
  Draw.draw_game_state gs timer; 
  Graphics.moveto 20 700;
  Graphics.synchronize ();
  let gs = 
    match Graphics.read_key () with 
    | '0' -> exit 0  
    | '1' -> gs
    | '2' -> update_difficulty gs Hard 
    | _ -> gs
  in
  (* let gs' = Game_state.new_level gs in  *)
  let timer = Timer.init_timer () in 
  let transition_gs = Game_state.new_level gs 
  in 
  Draw.draw_game_state transition_gs timer; 
  let levelup_gs = 
    match Graphics.read_key () with 
    | _ -> Game_state.new_level transition_gs
  in
  let timer = Timer.init_timer () in 
  Draw.draw_game_state levelup_gs timer; 
  run levelup_gs timer 
(* run gs' timer  *)

let main () = 
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  Graphics.set_window_title "Skedaddle Camel";
  Graphics.set_text_size 300;
  Graphics.moveto 20 700;
  Graphics.draw_string "press any key to start";
  Graphics.synchronize ();
  init ()

let () = main ()
