open Iterator
open Collisions

(*xmin xmax ymin ymax centre*)
type barre = float * float * float * float * float

let largeur = 80.0
let hauteur = 15.0
let y_fixe = 40.0

let creer x_souris (cadre_xmin, cadre_xmax) =
  let moitie = largeur /. 2.0 in
  let x = 
    if x_souris -. moitie < cadre_xmin then cadre_xmin +. moitie
    else if x_souris +. moitie > cadre_xmax then cadre_xmax -. moitie
    else x_souris 
  in ((x -. moitie), (x +. moitie), (y_fixe -. (hauteur /. 2.0)), (y_fixe +. (hauteur /. 2.0)), x)

let flux_barre flux_input cadre =
  Flux.map (fun (x_souris, _clic) -> creer x_souris cadre) flux_input

