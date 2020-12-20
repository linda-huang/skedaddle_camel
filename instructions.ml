(**********************************************************
   Instructional constants 
 ***********************************************************)
let welcome_txt = 
  ["The beginning of the maze is in the upper left corner; exit is the bottom right.";
   "Use WASD to move and press space to shoot projectiles.";
   "The projectiles will be shot in the same direction you are going.";
   "Avoid enemies! If you get too close, you die :( ";
   "There are 4 different tile types: regular, portal, mud, and ice.";
   "Portal tiles will teleport you to a corresponding portal tile";
   "Mud tiles will slow you down as you walk through them.";
   "Ice tiles are slippery! You'll speed up as you walk through them";
   "Careful! You won't be able to control your movements.";
   "Each level will have new elements and obstacles.";
   "If you need help at any time, press `i` for instructions";
   "Choose your level of difficulty to start playing! (default: easy)";
   "Press `1` for easy, press `2` for hard";]

let instructions_txt =
  ["Use WASD to control movement. Press space to shoot.";
   "If you get too close to an enemy, you will lose a life";
   "You can shoot walls (multiple times) to make a new path";
   "There are 4 different tile types: regular, portal, mud";
   "and ice.";
   "Portal tiles will teleport you to a corresponding portal tile";
   "Mud tiles will slow you down as you walk through them.";
   "Ice tiles are slippery!";
   "You'll speed up as you walk through them.";
   "Collect potions to regain health";
   "Collect coins to earn points";
   "Some levels contain a genie.";
   "Catch it if you can to earn extra points."]

let hourglass_txt = 
  ["There is an hourglass you can collect!";
   "It will be this color if it gives you 15 more seconds to complete the level.";
   "But sometimes it is extra powerful and will pause all enemies for the rest of the level.";
   "If it is this rare special hourglass, it will be a little magic wand"]

let genie_txt =  
  ["There are two potions you can collect to gain more health";
   "This level has 10 enemies";
   "There are two potions you can collect to gain more health";
   "There is a speedy genie in this maze! Catch it for extra points.";
   "The genie teleports sometimes :)"]