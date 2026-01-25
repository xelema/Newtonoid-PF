(* Logique principale du jeu *)

(* Etat complet du jeu *)
type etat = {
  ball_pos : float * float;
  ball_vel : float * float;
  bricks   : Brick.bricks;
  score    : int;
  vies     : int;
  niveau   : int;
  prev_barre_centre : float;
}

(* Constantes du jeu *)
val ball_radius : float
val g : float
val rayon_balle : float
val init_ball_pos : float * float
val init_ball_vel : float * float

(* Calcul d'une etape de trajectoire avec gravite *)
val calcul_step : float -> (float * float) * (float * float) -> (float * float) * (float * float)

(* Detection de collision avec une brique *)
val has_brick_collision : etat -> bool

(* Boucle principale du jeu - flux d'etats *)
val run : float -> etat -> Barreau.t Iterator.flux -> (etat * Barreau.t) Iterator.flux
