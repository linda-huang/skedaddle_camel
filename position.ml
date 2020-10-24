type t = {x : float; y : float}

let dist p1 p2 = 
  let sqr n = n *. n in 
  (sqr (p1.x -. p2.x)) +. (sqr (p1.y -. p2.y))
  |> sqrt 

let make_pos x y = 
  {x = x; y = y}

let make_pos_2 tuple = 
  {x = fst tuple; y = snd tuple}

let string_of_pos p = 
  "("  ^ string_of_float p.x ^ ", " ^ string_of_float p.y ^ ")"