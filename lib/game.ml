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
}

let ball_radius = 10.0

(*Integre un flux dans un autre flux*)
(*faut récréer un flux à chaque collision car calculs du flux plus bon *)
let integre dt flux v0 =

  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in

  let rec acc =
    Tick (lazy (Some (v0, Flux.map2 iter acc flux)))
  in acc;;


let g = 9.81 *. 50.(*consrtante gravité*)

let calcul_traj dt (pos,vit) =
  (*Accélération*)
  let flux_acc = Flux.constant (0.,-.g) in
  (*Vitesse*)
  let flux_vit = integre dt flux_acc vit in
  (*Position*)
  let flux_pos = integre dt flux_vit pos in

  Flux.map2 (fun p v -> (p,v)) flux_pos flux_vit

(* Détection de collision avec une brique *)
let has_brick_collision etat =
  Option.is_some (Collisions.find_colliding_brick (etat.ball_pos, etat.ball_vel) etat.bricks ball_radius)

(*prise en compte des rebonds sur la boite et les briques*)
let rec run dt etat =
  (* Flux de la trajectoire de la balle *)
  let ball_flux = calcul_traj dt (etat.ball_pos, etat.ball_vel) in

  (* Transformer en flux d'état *)
  let etat_flux = Flux.map (fun (p, v) -> { etat with ball_pos = p; ball_vel = v }) ball_flux in

  (* Gérer collisions avec les murs *)
  let after_walls = Flux.unless etat_flux
    (fun e -> Collisions.contact_boite (e.ball_pos, e.ball_vel))
    (fun e ->
      let (new_pos, new_vel) = Collisions.rebond_boite (e.ball_pos, e.ball_vel) in
      run dt { e with ball_pos = new_pos; ball_vel = new_vel }
    ) in

  (* Gérer collisions avec les briques *)
  Flux.unless after_walls
    has_brick_collision
    (fun e ->
      match Collisions.find_colliding_brick (e.ball_pos, e.ball_vel) e.bricks ball_radius with
      | None -> run dt e
      | Some brick ->
        let (new_pos, new_vel) = Collisions.rebond_brick (e.ball_pos, e.ball_vel) brick in
        let new_bricks = Brick.remove_brick brick e.bricks in
        let new_score = e.score + brick.value in
        run dt { ball_pos = new_pos; ball_vel = new_vel; bricks = new_bricks; score = new_score }
    )
