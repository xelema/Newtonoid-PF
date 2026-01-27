(* Configuration du jeu *)

(* Limites de la zone de jeu *)
type bounds = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
}

(* Parametres physiques *)
type physics = {
  gravity : float;
  ball_radius : float;
  acceleration_factor : float;
  max_speed : float;
  impulse_coefficient : float;
  perturbation : float;
  vx_max : float;
  v_securite : float;
}

(* Parametres de la raquette *)
type paddle = {
  width : float;
  height : float;
  y_position : float;
}

(* Parametres de la balle initiale *)
type ball = {
  initial_pos : Vec2.t;
  initial_vel : Vec2.t;
}

(* Configuration globale du jeu *)
type t = {
  bounds : bounds;
  physics : physics;
  paddle : paddle;
  ball : ball;
  initial_lives : int;
  dt : float;
}

let default_bounds = {
  x_min = 10.0;
  x_max = 790.0;
  y_min = 10.0;
  y_max = 590.0;
}

let default_physics = {
  gravity = 9.81 *. 100.;     
  ball_radius = 10.0;         
  acceleration_factor = 1.04;
  max_speed = 1000.0;         
  impulse_coefficient = 0.5; 
  perturbation = 0.15;        
  vx_max = 700.0;             
  v_securite = 600.0;       
}

let default_paddle = {
  width = 120.0;   
  height = 15.0; 
  y_position = 40.0; 
}

let default_ball = {
  initial_pos = Vec2.create 400. 100.; 
  initial_vel = Vec2.create 0. 600.;  
}

let default = {
  bounds = default_bounds;
  physics = default_physics;
  paddle = default_paddle;
  ball = default_ball;
  initial_lives = 3;
  dt = 1. /. 60.;
}

let make 
    ?(bounds = default_bounds)
    ?(physics = default_physics)
    ?(paddle = default_paddle)
    ?(ball = default_ball)
    ?(initial_lives = 3)
    ?(dt = 1. /. 60.)
    () =
  { bounds; physics; paddle; ball; initial_lives; dt }

(* Tests *)
let%test "Valeur valide default_bounds" =
  default_bounds.x_min < default_bounds.x_max &&
  default_bounds.y_min < default_bounds.y_max

let%test "Valeur valide gravity" =
  default_physics.gravity > 0.

let%test "Valeur valide ball_radius" =
  default_physics.ball_radius > 0.

let%test "Valeur valide default_paddle dimensions" =
  default_paddle.width > 0. && default_paddle.height > 0.

let%test "Valeur valide default_lives" =
  default.initial_lives > 0

let%test "Setup par défaut" =
  let cfg = make () in
  cfg.initial_lives = 3 && cfg.dt = default.dt

let%test "Setup avec vies choisies" =
  let cfg = make ~initial_lives:5 () in
  cfg.initial_lives = 5
