open Iterator
open Collisions
(*Meme definition qu'au TP7*)

(*x et y*)
type vect = (float * float)
(*l'etat c'est la position et la vitesse*)


type etat =  (float * float) * (float * float)

let g = 9.81 *. 50.(*consrtante gravité*)
let rayon_balle = 10.


let calcul_traj dt etat = 
  let (pos, vit) = etat in
  let (px, py) = pos in
  let (vx, vy) = vit in
  
  let (ax, ay) = (0., -.g) in
  
  let px_next = px +. vx *. dt +. 0.5 *. ax *. (dt *. dt) in
  let py_next = py +. vy *. dt +. 0.5 *. ay *. (dt *. dt) in
  
  let vx_next = vx +. ax *. dt in
  let vy_next = vy +. ay *. dt in

  ((px_next, py_next), (vx_next, vy_next))


let rec run dt etat_balle flux_barre =
  Tick (lazy (
    match Flux.uncons flux_barre with
    | None -> None
    | Some (barre, reste_barre) ->
       
       let etat_candidat = calcul_traj dt etat_balle in
       
       (* Detection collision *)
       let (xmin, xmax, ymin, ymax, centre) = barre in
       
      
       if Collisions.contact_boite etat_candidat || 
          Collisions.contact etat_candidat rayon_balle (xmin, xmax, ymin, ymax) 
       then
          let etat_rebond = 
            if Collisions.contact etat_candidat rayon_balle (xmin, xmax, ymin, ymax) then
              Collisions.rebond_barre etat_candidat (xmin, xmax, ymin, ymax, centre)
            else
              Collisions.rebond_boite etat_candidat
          in
          (* On continue*)
          Some ((etat_rebond, barre), run dt etat_rebond reste_barre)
       else
          (* Pas de collision*)
          Some ((etat_candidat, barre), run dt etat_candidat reste_barre)
  ))









