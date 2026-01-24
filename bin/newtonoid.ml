
open Libnewtonoid
open Iterator
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

(* Dessiner une brique *)
let draw_brick (brick : Brick.brick) =
  Graphics.set_color brick.color;
  Graphics.fill_rect
    (int_of_float brick.x)
    (int_of_float brick.y)
    (int_of_float brick.width)
    (int_of_float brick.height);
  Graphics.set_color Graphics.black;
  Graphics.draw_rect
    (int_of_float brick.x)
    (int_of_float brick.y)
    (int_of_float brick.width)
    (int_of_float brick.height)

(* Dessiner toutes les briques *)
let rec draw_bricks = function
  | Brick.Empty -> ()
  | Brick.Leaf l -> List.iter draw_brick l
  | Brick.Node (_, nw, ne, sw, se) ->
      draw_bricks nw; draw_bricks ne; draw_bricks sw; draw_bricks se

let draw_state (etat, barre) =
  draw_bricks etat.bricks;

  (* Dessiner la barre *)
  let (xmin, xmax, ymin, ymax, c) = barre in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 
    (int_of_float xmin) 
    (int_of_float ymin) 
    (int_of_float (xmax -. xmin)) 
    (int_of_float (ymax -. ymin));
  (* Dessiner la balle *)
  let (x, y) = etat.ball_pos in
  let int_x = int_of_float x in
  let int_y = int_of_float y in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle int_x int_y 10;
 
  (* Afficher niveau, score et vies *)
  Graphics.moveto 10 5;
  Graphics.draw_string (Format.sprintf "Niveau: %d  Score: %d  Vies: %d" 
                          (etat.niveau + 1) etat.score etat.vies)


let rec wait_for_restart_key () =
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  match ev.Graphics.key with
  | 'r' | 'R' -> true
  | 'q' | 'Q' -> false
  | _ -> wait_for_restart_key ()

let draw flux_etat =
  let rec loop flux_etat =
    match Flux.(uncons flux_etat) with
    | None -> 
        Graphics.clear_graph ();
        Graphics.set_color Graphics.red;
        Graphics.moveto 320 320;
        Graphics.draw_string "GAME OVER";
        Graphics.moveto 240 280;
        Graphics.draw_string "R = Recommencer | Q = Quitter";
        Graphics.synchronize ();
        wait_for_restart_key ()
    | Some ((etat, barre), flux_etat') ->
        Graphics.clear_graph ();
        draw_state (etat, barre);
        Graphics.synchronize ();
        Unix.sleepf Init.dt;
        loop flux_etat'
  in
  loop flux_etat

let () = 
  Graphics.open_graph " 800x600";
  Graphics.auto_synchronize false;

  let rec main_loop () =
    let dt = Init.dt in
    let init : Game.etat = {
      ball_pos = (400., 100.);
      ball_vel = (0., 500.);
      bricks = Layout.get_level 0;
      score = 0;
      vies = 3;
      niveau = 0;
      prev_barre_centre = 400.;
    } in
    let limites_cadre = (Box.infx, Box.supx) in
    let flux_barre = Barreau.flux_barre (Input.mouse ()) limites_cadre in
    let flux_jeu = Game.run dt init flux_barre in
    let restart = draw flux_jeu in
    if restart then main_loop ()
  in
  main_loop ();
  Graphics.close_graph ()
