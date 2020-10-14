type t = {x : float; y : float}

(* [dist p1 p2] is the euclidian distance between [p1] and [p2] *)
let dist p1 p2 = 
  let sqr n = n *. n in 
  (sqr (p1.x -. p2.x)) +. (sqr (p1.y -. p2.y))
  |> sqrt 

(* [make_pos x y] creates the position with coordinates (x,y) *)
let make_pos x y = 
  {x = x; y = y}