open Iterator

(* L'état ne contient pas la position/vitesse de la balle (c'est dans le flux de trajectoire) *)
type etat = {
  bricks : Brick.bricks;
  score  : int;
  vies   : int;
  niveau : int;
}

(* Constantes du jeu chargées a partir de la config *)
let ball_radius = Config.default_physics.ball_radius
let g = Config.default_physics.gravity
let rayon_balle = Config.default_physics.ball_radius

(* Position et vitesse initiales de la balle chargées a partir de la config *)
let init_ball_pos = Vec2.to_tuple Config.default_ball.initial_pos
let init_ball_vel = Vec2.to_tuple Config.default_ball.initial_vel

(* Calcul d'une étape de trajectoire sous gravité *)
let calcul_step dt (pos, vel) = 
  let (px, py) = pos in
  let (vx, vy) = vel in
  let (ax, ay) = (0., -.g) in
  let px_next = px +. vx *. dt +. 0.5 *. ax *. (dt *. dt) in
  let py_next = py +. vy *. dt +. 0.5 *. ay *. (dt *. dt) in
  let vx_next = vx +. ax *. dt in
  let vy_next = vy +. ay *. dt in
  ((px_next, py_next), (vx_next, vy_next))

(* Flux de trajectoire sous gravité*)
let flux_trajectoire dt pos_init vel_init =
  Flux.unfold
    (fun (pos, vel) ->
       let (new_pos, new_vel) = calcul_step dt (pos, vel) in
       Some ((pos, vel), (new_pos, new_vel)))
    (pos_init, vel_init)


(* Boucle principale récursive.
   flux_traj : flux de (pos, vel) - la trajectoire pure sous gravité
   flux_barre : flux de Barreau.t
   etat : état courant du jeu
   prev_centre : position précédente du centre de la barre *)
let rec flux_jeu dt flux_traj flux_barre etat prev_centre =
  Tick (lazy (
    match Flux.uncons flux_traj, Flux.uncons flux_barre with
    | None, _ | _, None -> None
    | Some ((pos, vel), reste_traj), Some (barre, reste_barre) ->
        (* Tester les conditions de collision dans l'ordre de priorité *)
        
        (* Balle tombée *)
        if snd pos < Config.default_bounds.y_min then
          if etat.vies <= 1 then
            None
          else
            (* Perd une vie *)
            let new_etat = { etat with vies = etat.vies - 1 } in
            let new_flux_traj = flux_trajectoire dt init_ball_pos init_ball_vel in
            Some ((pos, etat, barre), flux_jeu dt new_flux_traj reste_barre new_etat barre.Barreau.centre)
        
        (* Collision brique *)
        else if Option.is_some (Collisions.find_colliding_brick (pos, vel) etat.bricks ball_radius) then
          let brick = Option.get (Collisions.find_colliding_brick (pos, vel) etat.bricks ball_radius) in
          let (new_pos, new_vel) = Collisions.rebond_brick (pos, vel) brick in
          let new_bricks = Brick.remove_brick brick etat.bricks in
          let new_score = etat.score + brick.value in
          
          if Brick.is_empty new_bricks then
            (* Niveau terminé *)
            let next_niveau = etat.niveau + 1 in
            let new_etat = { bricks = Layout.get_level next_niveau; score = new_score;
                             vies = etat.vies; niveau = next_niveau } in
            let new_flux_traj = flux_trajectoire dt init_ball_pos init_ball_vel in
            Some ((pos, new_etat, barre), flux_jeu dt new_flux_traj reste_barre new_etat barre.Barreau.centre)
          else
            (* Collision brique *)
            let new_etat = { etat with bricks = new_bricks; score = new_score } in
            let new_flux_traj = flux_trajectoire dt new_pos new_vel in
            Some ((pos, new_etat, barre), flux_jeu dt new_flux_traj reste_barre new_etat barre.Barreau.centre)
        
        (* Collision barre *)
        else if snd vel < 0. && Collisions.contact (pos, vel) rayon_balle 
                  (barre.Barreau.xmin, barre.Barreau.xmax, barre.Barreau.ymin, barre.Barreau.ymax) then
          let centre = barre.Barreau.centre in
          let barre_vel = (centre -. prev_centre) /. dt in
          let (new_pos, new_vel) = Collisions.rebond_barre (pos, vel) barre_vel in
          let v_securite = Config.default_physics.v_securite in
          let final_vel = (fst new_vel, max (snd new_vel) v_securite) in
          let new_flux_traj = flux_trajectoire dt new_pos final_vel in
          Some ((pos, etat, barre), flux_jeu dt new_flux_traj reste_barre etat centre)
        
        (* Collision mur *)
        else if Collisions.contact_boite (pos, vel) then
          let (new_pos, new_vel) = Collisions.rebond_boite (pos, vel) in
          let new_flux_traj = flux_trajectoire dt new_pos new_vel in
          Some ((pos, etat, barre), flux_jeu dt new_flux_traj reste_barre etat barre.Barreau.centre)
        
        (* Pas de collision *)
        else
          Some ((pos, etat, barre), flux_jeu dt reste_traj reste_barre etat barre.Barreau.centre)
  ))

(* Point d'entrée : crée le flux initial et lance la boucle *)
let run dt etat_init flux_barre =
  let flux_traj = flux_trajectoire dt init_ball_pos init_ball_vel in
  let prev_centre = fst init_ball_pos in
  flux_jeu dt flux_traj flux_barre etat_init prev_centre

let%test "calcul_step pas de vitesse" =
  let dt = 0.1 in
  let ((px, _py), (vx, vy)) = calcul_step dt ((100., 100.), (0., 0.)) in
  Float.abs (px -. 100.) < 0.01 && 
  vx = 0. &&
  vy < 0.

let%test "calcul_step mouvement horizontal" =
  let dt = 0.1 in
  let ((px, _), (vx, _)) = calcul_step dt ((100., 100.), (100., 0.)) in
  px > 100. && Float.abs (vx -. 100.) < 0.01

let%test "calcul_step verttical avec gravité" =
  let dt = 0.1 in
  let ((_, py1), (_, vy1)) = calcul_step dt ((100., 100.), (0., 100.)) in
  let ((_, py2), (_, vy2)) = calcul_step dt ((100., 100.), (0., 0.)) in
  py1 > py2 && vy1 > vy2

let%test "calcul_step baisse vitesse verticale" =
  let dt = 0.1 in
  let initial_vy = 500. in
  let ((_, _), (_, vy)) = calcul_step dt ((100., 100.), (0., initial_vy)) in
  vy < initial_vy

let%test "init_ball_pos dans les bordures" =
  let (x, y) = init_ball_pos in
  x > 0. && x < 800. && y > 0. && y < 600.

let%test "init_ball_vel depart vers le haut" =
  let (_, vy) = init_ball_vel in
  vy > 0.


