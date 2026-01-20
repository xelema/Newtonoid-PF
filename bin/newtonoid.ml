(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let draw_state (etat,barre) = 
  let (x, y), (vitx, vity) = etat in
  let int_x = int_of_float x in
  let int_y = int_of_float y in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle int_x int_y 10;


  let (xmin, xmax, ymin, ymax, c) = barre in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 
    (int_of_float xmin) 
    (int_of_float ymin) 
    (int_of_float (xmax -. xmin)) 
    (int_of_float (ymax -. ymin)) 

(* extrait le score courant d'un etat : *)
let score etat : int = 0

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (last_score + score etat)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let () = 
  Graphics.open_graph " 800x600";

  let dt = Init.dt in
  let init = ((400.,500.),(250., 0.)) in
  let limites_cadre = (Box.infx, Box.supx) in
  let flux_barre = Barreau.flux_barre (Input.mouse ()) limites_cadre in

  let flux_jeu = Game.run dt init flux_barre in
  draw flux_jeu
