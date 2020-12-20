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

(** [respond_to_kp kp gs st camel] is [gs] updated in response to [kp] *)
let respond_to_kp kp (gs : Game_state.game_state) = 
  let camel = gs.round_state.camel in 
  let st = gs.round_state in 
  match kp with 
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

(** [wait_kp gs timer] updates [gs] and [timer] automatically 
    until a user presses a key *)
let rec wait_kp (gs : Game_state.game_state) (timer : Timer.timer) = 
  Unix.sleepf 0.001;
  if not (Graphics.key_pressed ()) then  
    let timer = Timer.update_timer timer in 
    let gs' = Game_state.update_game_state gs timer in 
    Draw.draw_game_state gs' timer;
    wait_kp gs' timer
  else gs

(** [exit_instructions gs timer i kp] is the appropriate response
    to [kp] when [gs] is in an instruction  *)
let exit_instructions gs timer i kp = 
  match kp with 
  | '0' -> exit 0  
  | 'x' -> let pause_dur = Unix.gettimeofday () -. i in 
    draw_initial_round_state gs.round_state gs.round_state.camel.coins;
    {gs with current_state = InPlay},
    {timer with totalpaused = timer.totalpaused +. pause_dur}
  | _ -> gs, timer

(** [input gs timer] updates [gs] in response to user key presses *)
let rec input (gs : Game_state.game_state) (timer : Timer.timer) 
  : (Game_state.game_state * Timer.timer) = 
  match gs.current_state with 
  | Instructions i -> exit_instructions gs timer i (Graphics.read_key ())
  | _ -> begin 
      let gs = wait_kp gs timer in  
      let timer = Timer.update_timer timer in
      let st = gs.round_state in 
      let camel = st.camel in 
      let gs' = if camel.last_tile <> Maze.Power_Path Ice 
        then respond_to_kp (Graphics.read_key ()) gs
        else {gs with round_state 
                      = {st with camel = Round_state.move_camel_ice st camel}} 
      in match gs'.current_state with 
      | Instructions i -> gs', timer
      | _ -> begin 
          let st' = gs'.round_state in 
          let st'' = if Round_state.hit_wall st' st'.camel.pos 
              st'.camel.dir Constant.camel_radius then st 
            else {st' with camel = (Round_state.hit_power_tile 
                                      st' st'.camel.pos)} in 
          {gs' with round_state = (update_round_state st'')}, 
          Timer.update_timer timer
        end 
    end 

(** [flush_keypress ()] clears the queue of key presses 
    from [Graphics.read_key ()] *)
let rec flush_keypress () = 
  if Graphics.key_pressed () 
  then (ignore (read_key ()); flush_keypress ();)
  else () 

(** [extract_wall_type maze col row] is the string representation of the 
    tile at column [col] and row [row] in [maze] *)
let extract_wall_type maze col row = 
  match Maze.tile_type maze col row with
  | Wall _ -> "wall"
  | Path -> "path"
  | Exit -> "exit"
  | Start -> "start" 
  | Power_Path Ice -> "ice path"
  | Power_Path Mud -> "mud path"
  | Power_Path Portal -> "portal path"

(** [run gs timer] runs the game across levels *)
let rec run (gs : Game_state.game_state) (oldtimer : Timer.timer) = 
  let newgs, timer = input gs oldtimer in 
  Draw.draw_game_state newgs timer;
  let coord_mapping = Position.pixel_to_tile gs.round_state.camel.pos 
      gs.round_state.top_left_corner in
  match coord_mapping with 
  | Position.Out_of_bounds -> Graphics.draw_string "out of bounds";
  | Valid (col, row) -> 
    if extract_wall_type gs.round_state.maze col row = "exit" 
    then transition_level gs timer 
    else run newgs timer 

(** [transition_level gs timer] goes to the next transition screen and then 
    waits until user presses [x] to go to next level after [gs] *)
and transition_level gs timer = 
  flush_keypress ();
  let transition_gs = Game_state.new_level gs in 
  Draw.draw_game_state transition_gs timer; 
  let rec go_to_nextlvl () = 
    match Graphics.read_key () with 
    | 'x' -> begin 
        let levelup_gs = Game_state.new_level transition_gs in 
        let timer = Timer.init_timer () in 
        draw_initial_round_state levelup_gs.round_state 0;
        Draw.draw_game_state levelup_gs timer; 
        match Graphics.read_key () with 
        | _ -> let timer = Timer.init_timer () in  run levelup_gs timer
      end 
    | _ -> go_to_nextlvl () in 
  go_to_nextlvl ()

(** [firstlevelup transition_gs] is the next level only 
    after the user inputs 'x' *)
let rec firstlevelup transition_gs = 
  match Graphics.read_key () with 
  | 'x' -> Game_state.new_level transition_gs
  | _ -> firstlevelup transition_gs 

(** [init ()] runs the main game past the intro page *)
let init prewelcome_gs = 
  let timer = Timer.init_timer () in 
  let gs = match Graphics.read_key () with 
    | _ -> Game_state.new_level prewelcome_gs in 
  Draw.draw_game_state gs timer;
  let gs = match Graphics.read_key () with 
    | '0' -> exit 0 
    | '1' -> gs 
    | '2' -> Game_state.update_difficulty gs Hard 
    | _ -> gs
  in
  let transition_gs = Game_state.new_level gs in 
  Draw.draw_game_state transition_gs timer; 
  (* move to first level upon keypress *)
  let levelup_gs = firstlevelup transition_gs in
  let timer = Timer.init_timer () in 
  draw_initial_round_state levelup_gs.round_state 0;
  Draw.draw_game_state levelup_gs timer; 
  run levelup_gs timer 

let main () = 
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  (* prewelcome screen *)
  let st = Round_state.init 21 21 5 1 2 in 
  let prewelcome_gs = Game_state.init st in 
  let timer = Timer.init_timer () in 
  Draw.draw_game_state prewelcome_gs timer; 
  Graphics.moveto 20 700;
  Graphics.synchronize ();
  init prewelcome_gs

let () = main ()