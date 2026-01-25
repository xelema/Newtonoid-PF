(* Module de la raquette du joueur *)

open Iterator

(* Type record pour la raquette *)
type t = {
  xmin : float;
  xmax : float;
  ymin : float;
  ymax : float;
  centre : float;
}

(* Dimensions par defaut de la raquette *)
let largeur = Config.default_paddle.width
let hauteur = Config.default_paddle.height
let y_fixe = Config.default_paddle.y_position

(* Cree une raquette a partir de la position x de la souris *)
let creer x_souris (cadre_xmin, cadre_xmax) =
  let moitie = largeur /. 2.0 in
  let x = 
    if x_souris -. moitie < cadre_xmin then cadre_xmin +. moitie
    else if x_souris +. moitie > cadre_xmax then cadre_xmax -. moitie
    else x_souris 
  in 
  {
    xmin = x -. moitie;
    xmax = x +. moitie;
    ymin = y_fixe -. (hauteur /. 2.0);
    ymax = y_fixe +. (hauteur /. 2.0);
    centre = x;
  }

(* Cree un flux de raquettes a partir du flux d'input souris *)
let flux_barre flux_input cadre =
  Flux.map (fun (x_souris, _clic) -> creer x_souris cadre) flux_input

(* Tests *)
let%test "creer centre" =
  let b = creer 400. (10., 790.) in
  b.centre = 400.

let%test "creer clamp left" =
  let b = creer 0. (10., 790.) in
  b.xmin >= 10.

let%test "creer clamp right" =
  let b = creer 800. (10., 790.) in
  b.xmax <= 790.

let%test "creer dimensions" =
  let b = creer 400. (10., 790.) in
  Float.abs ((b.xmax -. b.xmin) -. largeur) < 0.001

let%test "creer y position" =
  let b = creer 400. (10., 790.) in
  let mid_y = (b.ymin +. b.ymax) /. 2.0 in
  Float.abs (mid_y -. y_fixe) < 0.001
