open Graphics
open Camel
open Enemy
open Maze 
open Round_state      

(* let fightingring = 
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
   (** [is_dead camel] is if [camel] has run out of health *)
   let is_dead camel = camel.health = 0

   let at_exit (st : State.t) = 
   let camel = st.camel in 
   let (x, y) = State.curr_tile camel.pos in 
   Maze.tile_type st.maze x y = Exit 

   (* [update_camel st] is the state with the camel's 
   health and coin total updated *)
   let update_camel (st : State.t) : Round_S = 
   let camel = st.camel in 
   let camel' = if (State.near_enemy camel st) then 
      {camel with health = camel.health - 1} else camel in 
   let camel'' = if (State.on_coin camel st) then 
      {camel with coins = camel.coins + 1} else camel' in 
   if (is_dead camel'') then failwith " game over " else
    {st with camel = camel''}  

   (** [get_coin st] is [st] with the coin the camel is currently on removed *)
   let get_coin (st : State.t) : Round_S = 
   let c = find_coin st.camel.pos st in 
   rem_coin c st 

   (** [update_state st] is [st] with all agents updated one move
    e.g. all enemies moved one step; projectiles moved one unit; 
    any applicable coins picked up; camel score and health adjusted *)
   let update_state (st : Round_S) : Round_R = 
   R   let st' = st |> update_camel |> Round_stateproj |> Round_state.Semies in 
   if (Round_statein st'.camel st') then get_coin st' else st'

   (** [draw round_states the Graphics representation of [st] *)
   let draw round_statefailwith "todo" 

   (** [input st k] updates [st] in response to [k].
    It ends the game when [k] = '0' *)
   let input (st : Round_state : char) : Round_state.S   Graphics.clear_graph ();
   Graphics.moveto 50 500;
   let camel = st.camel in 
   let st' = 
    match k with 
    | '0' -> exit 0 
    | 'w' -> {st with camel = (Camel.move_vert camel 1)}
    | 'a' -> {st with camel = (Camel.move_horiz camel ~-1)}
    | 's' -> {st with camel = (Camel.move_vert camel ~-1)}
    | 'd' -> {st with camel = (Camel.move_horiz camel 1)}
    | 'e' -> {st with camel = (Camel.turn_right camel)}
    | 'q' -> {st with camel = (Camel.turn_left camel)}
    | ' ' -> Round_state camel st
    | _ -> {st with camel = camel} 
   in 
   let st'' = if Round_stateall st'.camel.pos st'.maze then st else st' in 
   st'' |> Round_stateproj |> Round_state.Semies 

   (** [run st] runs the game responding to key presses *)
   let rec run (st : Round_state
   Graphics.moveto 50 500;
   Graphics.draw_string "press a key to move (press 0 to exit)";
   let s = wait_next_event[Key_pressed] in 
   if s.keypressed then 
    (Graphics.clear_graph ();
     let newst = input st s.key in 
     Graphics.moveto 50 400;
     Graphics.draw_string ("Began as: " ^ Round_stateg_of_round round_state     Graphics.moveto 50 300;
     Graphics.draw_string ("Moved to: " ^ Rtate.round_stateg_of_round round_state);
     Graphics.moveto 50 200;
     if at_exit newst then 
       (Graphics.clear_graph ();
        Graphics.moveto 50 550;
        Graphics.draw_string "welcome to a new maze!"; 
        let camel = Camel.init 0 0 in 
        let st = Round_round_state.init camel 10 10 5 in 
        run st)
     else run newst)

   (** [init k] creates a new game Round_round_state with camel initialized at the origin
    in a maze of dimensions 10x10 and then runs the game *)
   let init k = 
   let camel = Camel.init 0 0 in 
   let st = round_state.init camel 11 11 5 in 
   run st 

   (* Start on key press *)
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
   let () = main () *)
(* Execute the demo. *)
(* let () = main ()  *)
