(* Module de la raquette du joueur *)

(* Type de la raquette avec tous ses parametres *)
type t = {
  xmin : float;
  xmax : float;
  ymin : float;
  ymax : float;
  centre : float;
}

(* Dimensions de la raquette *)
val largeur : float
val hauteur : float
val y_fixe : float

(* Cree une raquette a partir de la position x de la souris et des limites du cadre *)
val creer : float -> (float * float) -> t

(* Cree un flux de raquettes a partir du flux d'input souris *)
val flux_barre : (float * bool) Iterator.flux -> (float * float) -> t Iterator.flux
