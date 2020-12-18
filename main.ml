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

(** [input gs timer] updates [gs] in response to user key presses *)
let rec input (gs : Game_state.game_state) (timer : Timer.timer) 
  : (Game_state.game_state * Timer.timer) = 
  match gs.current_state with 
  | Instructions i -> begin 
      (* return to level play upon 'x' input *)
      match Graphics.read_key () with 
      | '0' -> exit 0  
      | 'x' -> let pause_dur = Unix.gettimeofday () -. i in 
        {gs with current_state = InPlay},
        {timer with totalpaused = timer.totalpaused +. pause_dur}
      | _ -> gs, timer
    end 
  | _ -> begin 
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
      let timer = Timer.update_timer timer in
      let camel = gs.round_state.camel in 
      let st = gs.round_state in 
      let gs' = 
        match Graphics.read_key () with 
        | '0' -> exit 0  
        | 'i' -> {gs with current_state = Instructions (Unix.gettimeofday ())}
        | 'w' -> {gs with round_state = 
                            {st with camel = (Camel.move_vert camel 1 'w')}}
        | 'a' -> {gs with round_state = 
                            {st with camel = (Camel.move_horiz camel ~-1 'a')}}
        | 's' -> {gs with round_state = 
                            {st with camel = (Camel.move_vert camel ~-1 's')}}
        | 'd' -> {gs with round_state = 
                            {st with camel = (Camel.move_horiz camel 1 'd')}}
        | 'e' -> {gs with round_state = 
                            {st with camel = (Camel.turn_right camel)}}
        | 'q' -> {gs with round_state = 
                            {st with camel = (Camel.turn_left camel)}}
        | ' ' -> {gs with round_state = 
                            shoot camel st}
        | _ -> {gs with round_state = 
                          {st with camel = camel}} 
      in
      match gs'.current_state with 
      | Instructions i -> gs', timer
      | _ -> begin 
          let st' = gs'.round_state in 
          let st'' = if Round_state.hit_wall st' st'.camel.pos 
              st'.camel.dir Constant.camel_radius
            then st else st' in 
          let finst = st'' |> update_round_state in 
          {gs' with round_state = finst}, Timer.update_timer timer
        end 
    end 

(** [flush_keypress ()] clears the queue of key presses 
    from [Graphics.read_key ()] *)
let rec flush_keypress () = 
  if Graphics.key_pressed () 
  then (ignore (read_key ()); flush_keypress ();)
  else () 

(** [run gs timer] runs the game across levels *)
let rec run (gs : Game_state.game_state) (oldtimer : Timer.timer) = 
  let newgs, timer = input gs oldtimer in 
  Draw.draw_game_state newgs timer;
  let coord_mapping = Position.pixel_to_tile gs.round_state.camel.pos 
      gs.round_state.top_left_corner in
  match coord_mapping with 
  | Position.Out_of_bounds -> Graphics.draw_string "out of bounds";
  | Valid (col, row) -> 
    let extract_wall_type maze col row = 
      match Maze.tile_type maze col row with
      | Wall _ -> "wall"
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
            (* Unix.sleep 1; *)
            match Graphics.read_key () with 
            | _ -> let timer = Timer.init_timer () in  
              run levelup_gs timer
          end 
        | _ -> go_to_nextlvl () in 
      go_to_nextlvl ()
    end 
    else run newgs timer 

(** [init ()] creates a new Round_state and runs the game *)
let init () = 
  (* prewelcome screen *)
  let st = Round_state.init 21 21 5 1 in 
  let prewelcome_gs = Game_state.init st in 
  let timer = Timer.init_timer () in 
  Draw.draw_game_state prewelcome_gs timer; 
  Graphics.moveto 20 700;
  Graphics.synchronize ();
  (* move to welcome screen upon keypress *)
  let gs = match Graphics.read_key () with 
    | _ -> Game_state.new_level prewelcome_gs in 
  Draw.draw_game_state gs timer;
  (* move to transition 1 screen upon keypress *)
  let gs = 
    match Graphics.read_key () with 
    | '0' -> exit 0  
    | '1' -> gs
    | '2' -> Game_state.update_difficulty gs Hard 
    | _ -> gs
  in
  let timer = Timer.init_timer () in 
  let transition_gs = Game_state.new_level gs in 
  Draw.draw_game_state transition_gs timer; 
  (* move to first level upon keypress *)
  let levelup_gs = 
    match Graphics.read_key () with 
    | _ -> Game_state.new_level transition_gs
  in
  let timer = Timer.init_timer () in 
  Draw.draw_game_state levelup_gs timer; 
  run levelup_gs timer 

let main () = 
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  init ()

let () = main ()
