open Iterator
open Collisions
(*Meme definition qu'au TP7*)

(*x et y*)
type vect = (float * float)

(* État du jeu incluant les briques *)
type etat = {
  ball_pos : float * float;
  ball_vel : float * float;
  bricks   : Brick.bricks;
  score    : int;
  vies     : int;
  niveau   : int;
  prev_barre_centre : float;
}

let ball_radius = 10.0

let g = 9.81 *. 50. (*constante gravité*)
let rayon_balle = 10.

(* Position et vitesse initiales de la balle *)
let init_ball_pos = (400., 300.)
let init_ball_vel = (0., 600.)


(* Calcul de la prochaine étape de trajectoire *)
let calcul_step dt (pos, vit) = 
  let (px, py) = pos in
  let (vx, vy) = vit in
  let (ax, ay) = (0., -.g) in
  let px_next = px +. vx *. dt +. 0.5 *. ax *. (dt *. dt) in
  let py_next = py +. vy *. dt +. 0.5 *. ay *. (dt *. dt) in
  let vx_next = vx +. ax *. dt in
  let vy_next = vy +. ay *. dt in
  ((px_next, py_next), (vx_next, vy_next))

(* Détection de collision avec une brique *)
let has_brick_collision etat =
  Option.is_some (Collisions.find_colliding_brick (etat.ball_pos, etat.ball_vel) etat.bricks ball_radius)

let rec run dt etat flux_barre =
  Tick (lazy (
    match Flux.uncons flux_barre with
    | None -> None
    | Some (barre, reste_barre) ->
       let (xmin, xmax, ymin, ymax, centre) = barre in
       (* Calculer la prochaine position de la balle *)
       let (pos_next, vel_next) = calcul_step dt (etat.ball_pos, etat.ball_vel) in
       let (_, y_next) = pos_next in

       (*Si la balle tombe*)
       if y_next < 10.0 then
         if etat.vies <= 0 then
           None (*Game over*)
         else
           (* On perd une vie et on replace la balle au centre *)
            let etat_reset = { etat with
              vies = etat.vies - 1;
              ball_pos = init_ball_pos;
              ball_vel = init_ball_vel;
              prev_barre_centre = centre
            } in
           Some ((etat_reset, barre), run dt etat_reset reste_barre)
       else
          let etat_next = { etat with ball_pos = pos_next; ball_vel = vel_next; prev_barre_centre = centre } in
       
          (* Vérifier d'abord les collisions avec les briques *)
          let collision_brick = Collisions.find_colliding_brick (pos_next, vel_next) etat.bricks ball_radius in
       
          match collision_brick with
          | Some brick ->
              (* Collision avec une brique *)
              let (new_pos, new_vel) = Collisions.rebond_brick (pos_next, vel_next) brick in
              let new_bricks = Brick.remove_brick brick etat.bricks in
              let new_score = etat.score + brick.value in
              
              (* Verifier si toutes les briques sont cassees (victoire niveau) *)
              if Brick.is_empty new_bricks then
                (* Passer au niveau suivant *)
                let next_niveau = etat.niveau + 1 in
                let nouvel_etat = {
                  ball_pos = init_ball_pos;
                  ball_vel = init_ball_vel;
                  bricks = Layout.get_level next_niveau;
                  score = new_score;
                  vies = etat.vies;
                  niveau = next_niveau;
                  prev_barre_centre = centre;
                } in
                Some ((nouvel_etat, barre), run dt nouvel_etat reste_barre)
              else
                let nouvel_etat = {
                  ball_pos = new_pos;
                  ball_vel = new_vel;
                  bricks = new_bricks;
                  score = new_score;
                  vies = etat.vies;
                  niveau = etat.niveau;
                  prev_barre_centre = centre;
                } in
                Some ((nouvel_etat, barre), run dt nouvel_etat reste_barre)
          | None ->
              (* Pas de collision avec les briques, vérifier boite et barre *)
              if Collisions.contact_boite (pos_next, vel_next) then
                (* Collision avec les murs *)
                let (new_pos, new_vel) = Collisions.rebond_boite (pos_next, vel_next) in
                let nouvel_etat = { etat with ball_pos = new_pos; ball_vel = new_vel; prev_barre_centre = centre } in
                Some ((nouvel_etat, barre), run dt nouvel_etat reste_barre)
              else if Collisions.contact (pos_next, vel_next) rayon_balle (xmin, xmax, ymin, ymax) then
                (* Collision avec la barre *)
                let barre_vel = (centre -. etat.prev_barre_centre) /. dt in
                let (new_pos, new_vel) = Collisions.rebond_barre (pos_next, vel_next) barre_vel in
                let v_securite = 500.0 in (*ptite impulsion verticale garantie sur le contact avec la barre*)
                let final_vel = (fst new_vel, max (snd new_vel) v_securite) in
                let nouvel_etat = { etat with ball_pos = new_pos; ball_vel = final_vel; prev_barre_centre = centre } in
                Some ((nouvel_etat, barre), run dt nouvel_etat reste_barre)
              else
                (* Pas de collision *)
                Some ((etat_next, barre), run dt etat_next reste_barre)
  ))


