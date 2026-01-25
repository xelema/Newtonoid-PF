(* Detection et gestion des collisions *)

(* Limites de la zone de jeu *)
val x_min : float
val x_max : float
val y_min : float
val y_max : float

(* Detection de collision avec les bords de la fenetre *)
val contact_boite : (float * float) * (float * float) -> bool

(* Calcul du rebond sur les bords *)
val rebond_boite : (float * float) * (float * float) -> (float * float) * (float * float)

(* Detection de collision cercle/rectangle (balle/barre ou balle/brique) *)
val contact : (float * float) * (float * float) -> float -> (float * float * float * float) -> bool

(* Rebond generique sur un rectangle *)
val rebond : (float * float) * (float * float) -> (float * float * float * float) -> (float * float) * (float * float)

(* Collision cercle (balle) avec AABB (brique) - algorithme efficace *)
val circle_aabb_contact : float * float -> float -> Brick.box -> bool

(* Recherche d'une brique en collision dans le quadtree *)
val find_colliding_brick : (float * float) * (float * float) -> Brick.bricks -> float -> Brick.brick option

(* Calcul du rebond sur une brique *)
val rebond_brick : (float * float) * (float * float) -> Brick.brick -> (float * float) * (float * float)

(* Rebond sur la raquette avec impulsion *)
val rebond_barre : (float * float) * (float * float) -> float -> (float * float) * (float * float)
