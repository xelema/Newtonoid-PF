open Libnewtonoid
open Iterator
open Game

(* Configuration du jeu *)
let config = Config.default

(* Calcul du format de la fenetre graphique *)
let graphic_format =
  let bounds = config.Config.bounds in
  let width = int_of_float (bounds.x_max -. bounds.x_min +. 20.) in
  let height = int_of_float (bounds.y_max -. bounds.y_min +. 20.) in
  Format.sprintf " %dx%d+50+50" width height

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

(* Dessiner toutes les briques (parcours du quadtree) *)
let rec draw_bricks = function
  | Brick.Empty -> ()
  | Brick.Leaf l -> List.iter draw_brick l
  | Brick.Node (_, nw, ne, sw, se) ->
      draw_bricks nw; draw_bricks ne; draw_bricks sw; draw_bricks se

(* Dessiner l'etat complet du jeu *)
let draw_state (etat, barre) =
  draw_bricks etat.bricks;

  (* Dessiner la raquette *)
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 
    (int_of_float barre.Barreau.xmin) 
    (int_of_float barre.Barreau.ymin) 
    (int_of_float (barre.Barreau.xmax -. barre.Barreau.xmin)) 
    (int_of_float (barre.Barreau.ymax -. barre.Barreau.ymin));

  (* Dessiner la balle *)
  let (x, y) = etat.ball_pos in
  let radius = int_of_float config.physics.ball_radius in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (int_of_float x) (int_of_float y) radius;
 
  (* Afficher niveau, score et vies *)
  Graphics.moveto 10 5;
  Graphics.draw_string (Format.sprintf "Niveau: %d  Score: %d  Vies: %d" 
                          (etat.niveau + 1) etat.score etat.vies)

(* Attendre une touche pour redemarrer ou quitter *)
let rec wait_for_restart_key () =
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  match ev.Graphics.key with
  | 'r' | 'R' -> true
  | 'q' | 'Q' -> false
  | _ -> wait_for_restart_key ()

(* Boucle de rendu principale *)
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
        Unix.sleepf config.dt;
        loop flux_etat'
  in
  loop flux_etat

(* Point d'entree principal *)
let () = 
  Random.self_init ();
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;

  let rec main_loop () =
    (* Creer l'etat initial du jeu *)
    let init : Game.etat = {
      ball_pos = Game.init_ball_pos;
      ball_vel = Game.init_ball_vel;
      bricks = Layout.get_level 0;
      score = 0;
      vies = config.initial_lives;
      niveau = 0;
      prev_barre_centre = fst Game.init_ball_pos;
    } in
    (* Limites du cadre pour la raquette *)
    let limites_cadre = (config.bounds.x_min, config.bounds.x_max) in
    (* Creer les flux *)
    let flux_barre = Barreau.flux_barre (Input.mouse ()) limites_cadre in
    let flux_jeu = Game.run config.dt init flux_barre in
    (* Lancer la boucle de jeu *)
    let restart = draw flux_jeu in
    if restart then main_loop ()
  in
  main_loop ();
  Graphics.close_graph ()
