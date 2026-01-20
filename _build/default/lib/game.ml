open Iterator
open Collisions
(*Meme definition qu'au TP7*)

(*x et y*)
type vect = (float * float)
(*l'etat c'est la position et la vitesse*)


type etat =  (float * float) * (float * float)

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

(*prise en conmpte des rebond seulement de la boite pour le moment*)
let rec run dt (pos,vit) =
  Flux.unless (calcul_traj dt (pos,vit)) Collisions.contact_boite (fun etat_actuel ->
    let nouvel_etat = Collisions.rebond_boite etat_actuel in
    run dt nouvel_etat)











