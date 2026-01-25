(* Logique principale du jeu *)

(* Etat du jeu - ne contient PAS la position/vitesse de la balle (c'est dans le flux) *)
type etat = {
  bricks : Brick.bricks;
  score  : int;
  vies   : int;
  niveau : int;
}

(* Constantes du jeu *)
val ball_radius : float
val g : float
val rayon_balle : float
val init_ball_pos : float * float
val init_ball_vel : float * float

(* Calcul d'une etape de trajectoire avec gravite *)
val calcul_step : float -> (float * float) * (float * float) -> (float * float) * (float * float)

(* Flux de trajectoire de la balle (positions/vitesses successives sous gravite) *)
val flux_trajectoire : float -> float * float -> float * float -> ((float * float) * (float * float)) Iterator.flux

(* Boucle principale du jeu - retourne un flux de (ball_pos, etat, barre) *)
val run : float -> etat -> Barreau.t Iterator.flux -> ((float * float) * etat * Barreau.t) Iterator.flux
