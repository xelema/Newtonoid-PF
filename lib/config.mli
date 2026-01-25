(*  Configuration du jeu *)

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

(* Valeurs par defaut *)
val default_bounds : bounds
val default_physics : physics
val default_paddle : paddle
val default_ball : ball
val default : t

(* Constructeur avec valeurs par defaut *)
val make : 
  ?bounds:bounds ->
  ?physics:physics ->
  ?paddle:paddle ->
  ?ball:ball ->
  ?initial_lives:int ->
  ?dt:float ->
  unit -> t
