open Round_state
open Constant

type game_end = Time | Health 

type state = 
  | PreWelcome
  | Welcome 
  | GameOver of game_end
  | Won 
  | InPlay
  | Transition of int 
  | Instructions of float 

type difficulty = Easy | Hard 

type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t;
  game_difficulty : difficulty;
}

let int_of_difficulty diff = 
  match diff with 
  | Easy -> 1
  | Hard -> 2

let helper_handle_welcome (gs : game_state) : game_state = 
  {gs with current_state = Transition 0; 
           round_state = Round_state.init 
               Constant.round1.dimx Constant.round1.dimy 
               Constant.round1.enemies 
               (int_of_difficulty gs.game_difficulty)
               Constant.round1.portals } 

let helper_handle_catchall (gs : game_state) : game_state =
  let newscr = 
    Scorer.update_score gs.score (Sys.time ()) gs.round_state.camel in
  if gs.score.mazes = Constant.totrounds - 1 then 
    {gs with current_state = Won; score = newscr} 
  else if Camel.is_dead gs.round_state.camel then 
    {gs with current_state = GameOver Health; score = newscr} 
  else 
    let round, transition_num = 
      if gs.score.mazes = 0 
      then Constant.round2, 1 
      else Constant.round3, 2 in  
    let newstate = 
      Round_state.init round.dimx round.dimy 
        round.enemies (int_of_difficulty gs.game_difficulty)
        round.portals in 
    {gs with score = newscr; 
             current_state = Transition transition_num;
             round_state = newstate}

let new_level (gs : game_state) : game_state = 
  match gs.current_state with 
  | PreWelcome -> {gs with current_state = Welcome}
  | Welcome -> helper_handle_welcome gs
  | Transition t -> {gs with current_state = InPlay}
  | _ ->  helper_handle_catchall gs

let helper_handle_hard (gs : game_state) (st : Round_state.t) 
    (timer : Timer.timer) (curr_round : Constant.round_info) : game_state =
  if Camel.is_dead st.camel  
  then {gs with current_state = GameOver Health; 
                round_state = st} 
  else if Timer.out_of_time curr_round gs.round_state timer  
  then {gs with current_state = GameOver Time; 
                round_state = st} 
  else gs

let helper_handle_easy (gs : game_state) (st : Round_state.t): game_state = 
  if Camel.is_dead st.camel 
  then {gs with current_state = GameOver Health; 
                round_state = st} 
  else gs

let update_game_state (gs : game_state) (timer : Timer.timer): game_state = 
  let st = Round_state.update_round_state gs.round_state in 
  let curr_round = 
    if gs.score.mazes = 0 then Constant.round1 
    else if gs.score.mazes = 1 then Constant.round2 
    else Constant.round3 in 
  let enemies_hit = 
    Array.length gs.round_state.enemies - Array.length st.enemies in 
  let gs = {gs with round_state = st; 
                    score = {gs.score with 
                             hit = gs.score.hit + enemies_hit}} in
  match gs.game_difficulty with 
  | Easy -> helper_handle_easy gs st
  | Hard -> helper_handle_hard gs st timer curr_round 

let update_difficulty gs diff = 
  {gs with game_difficulty = diff}

let init (st : Round_state.t) : game_state = 
  {score = Scorer.init (); 
   current_state = PreWelcome; 
   round_state = st;
   game_difficulty = Easy}

let string_of_game_state (gs : game_state) : string = 
  let msg = match gs.current_state with 
    | PreWelcome -> "SkedaddleCamel"
    | Welcome -> "Welcome!"
    | InPlay -> "Game in progress"
    | GameOver overmsg -> begin 
        match overmsg with 
        | Time -> "Game over: time"
        | Health -> "Game over: health" 
      end 
    | Won -> "You've won!"
    | Transition t -> "Transition " ^ string_of_int t
    | Instructions i -> "Instructions" 
  in 
  let score = match gs.game_difficulty with 
    | Easy -> Scorer.string_of_score gs.score gs.round_state.camel false
    | Hard -> Scorer.string_of_score gs.score gs.round_state.camel true 
  in 
  msg ^ score  ^ " \ " ^ Round_state.string_of_round_state gs.round_state  