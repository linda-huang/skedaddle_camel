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
let input (st : Round_state.t) (scr : Scorer.t) : Round_state.t = 
  (* while not key pressed
     if Unix.time mod 1000 (some small time increment?) then move all proj and enemies *)
  let rec wait_kp st = 
    if not (Graphics.key_pressed ()) then  
      (* let st' = st |> State.move_proj |> Round_Rove_enemies in  *)
      let st' = update_round_state st scr in 
      Draw.draw_round_state st';
      Graphics.synchronize ();
      wait_kp st' 
    else st 
  in 
  let st = wait_kp st in  
  let camel = st.camel in 
  let k = Graphics.read_key () in 
  let st' = 
    match k with 
    | '0' -> exit 0  
    | 'w' -> {st with camel = (Camel.move_vert camel 1)}
    | 'a' -> {st with camel = (Camel.move_horiz camel ~-1)}
    | 's' -> {st with camel = (Camel.move_vert camel ~-1)}
    | 'd' -> {st with camel = (Camel.move_horiz camel 1)}
    | 'e' -> {st with camel = (Camel.turn_right camel)}
    | 'q' -> {st with camel = (Camel.turn_left camel)}
    | ' ' -> shoot camel st
    | _ -> {st with camel = camel} 
  in 
  let st'' = if hit_wall st' st'.camel.pos
    then st else st' in 
  (* Draw.draw_round_state st'';
     Graphics.synchronize (); *)
  st'' |> move_proj |> move_enemies 

(** [run st] runs the game responding to key presses *)
let rec run (st : Round_state.t) (scr : Scorer.t) = 
  (* Graphics.open_graph " "; *)
  Graphics.moveto 50 800; 
  Graphics.draw_string (string_of_float (Sys.time ()));
  Graphics.moveto 50 700;
  Graphics.draw_string "press a key to move (press 0 to exit)";
  (* let new_level st scr =
     (Graphics.clear_graph ();
     Graphics.moveto 50 550;
     Graphics.draw_string "welcome to a new maze!"; 
     let camel = Camel.init 0. 0. in 
     let st = State.init camel 10 10 5 in 
     let scr' = Scorer.update_time scr (Sys.time ()) in 
     run st scr') in *)
  let newst = input st scr in 
  (* let newst = st in  *)
  ( 
    Draw.draw_round_state newst;
    Graphics.set_color Graphics.black; 
    Graphics.moveto 50 750;
    (* Graphics.draw_string ("Began as: " ^ Round_state.string_of_round_state st); *)
    Graphics.moveto 50 725;
    (* Graphics.draw_string ("Moved to: " ^ Round_state.string_of_round_state newst); *)
    (* if at_exit newst then new_level st scr
       else  *)
    run newst scr)

(** [init k] creates a new game round_state with camel initialized at the origin
    in a maze of dimensions 10x10 and then runs the game *)
let init () = 
  let st = Round_state.init 43 43 5 in 
  let scr = Scorer.init () in 
  draw_round_state st; 
  Graphics.moveto 20 700;
  Graphics.synchronize ();
  (* Graphics.draw_string "check init"; *)
  run st scr

(* Start on key press *)
let main () = 
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  Graphics.set_window_title "Skedadle Camel";
  (* Graphics.resize_window window_width window_height; *)
  Graphics.set_text_size 300;
  Graphics.moveto 20 700;
  Graphics.draw_string "press any key to start";
  Graphics.synchronize ();

  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then init ()

(* Execute the demo. *)
let () = main ()
