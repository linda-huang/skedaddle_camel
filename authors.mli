(** Information about authors and hours worked *)

(** [net_ids] is the list of team members' net ids *)
val net_ids : string list 

(** [hours_worked] is the number of hours worked on this assignment 
    by each team member, in the order the [net_ids]. *)
val hours_worked : int list