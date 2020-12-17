open Round_state
open Constant

type game_end = Time | Health 

type state = 
  | Welcome 
  | GameOver of game_end
  | Won 
  | InPlay
  | Transition of int 
  | Instructions of int

type difficulty = Easy | Hard 

type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t;
  game_difficulty : difficulty;
}

let set_game_state g s = { g with current_state = s }

let get_game_state g = g.current_state

let int_of_difficulty diff = 
  match diff with 
  | Easy -> 1
  | Hard -> 2

let new_level (gs : game_state) : game_state = 
  match gs.current_state with 
  | Welcome -> begin 
      {gs with current_state = Transition 0; 
               round_state = Round_state.init 
                   Constant.round1.dimx Constant.round1.dimy 
                   Constant.round1.enemies
                   (int_of_difficulty gs.game_difficulty)} 
    end 
  | Transition t -> {gs with current_state = InPlay}
  | _ -> begin 
      let newscr = 
        Scorer.update_score gs.score (Sys.time ()) gs.round_state.camel in
      if gs.score.mazes = Constant.totrounds-1 then 
        {gs with current_state = Won; score = newscr} 
      else if Camel.is_dead gs.round_state.camel then 
        {gs with current_state = GameOver Health; score = newscr} 
      else 
        (* move to the next level *)
        let round, transition_num = 
          if gs.score.mazes = 0 
          then Constant.round2, 1 
          else Constant.round3, 2 in  
        let newstate = 
          Round_state.init round.dimx round.dimy 
            round.enemies (int_of_difficulty gs.game_difficulty) in 
        {gs with score = newscr; 
                 current_state = Transition transition_num;
                 round_state = newstate}
    end 

let update_game_state (gs : game_state) (timer : Timer.timer): game_state = 
  let st = Round_state.update_round_state gs.round_state in 
  let curr_round = 
    if gs.score.mazes = 0 then Constant.round1 
    else if gs.score.mazes = 1 then Constant.round2 
    else Constant.round3 in 
  let enemies_hit = 
    (Array.length gs.round_state.enemies) - (Array.length st.enemies) in 
  let gs = {gs with round_state = st; 
                    score = {gs.score with 
                             hit = gs.score.hit + enemies_hit}} in
  match gs.game_difficulty with 
  | Easy -> begin 
      if Camel.is_dead st.camel 
      then {gs with current_state = GameOver Health; 
                    round_state = st} 
      else gs
    end 
  | Hard -> begin 
      if Camel.is_dead st.camel  
      then {gs with current_state = GameOver Health; 
                    round_state = st} 
      else if Timer.out_of_time curr_round gs.round_state timer  
      then {gs with current_state = GameOver Time; 
                    round_state = st} 
      else gs
    end 

let update_difficulty gs diff = 
  {gs with game_difficulty = diff}

let init (st : Round_state.t) : game_state = 
  {score = Scorer.init (); 
   current_state = Welcome; 
   round_state = st;
   game_difficulty = Easy}

let string_of_game_state (gs : game_state) : string = 
  let msg = match get_game_state gs with 
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