# AGENTS.md - Guide pour les agents de codage

Ce fichier contient les instructions pour les agents de codage travaillant sur le projet Newtonoid.

## Vue d'ensemble du projet

Newtonoid est un jeu de casse-brique (style Arkanoid) implemente en OCaml avec une approche fonctionnelle. Le projet utilise:
- **Dune** comme systeme de build
- **Graphics** pour l'affichage
- **ppx_inline_test** pour les tests
- **Flux (streams)** pour representer l'evolution de l'etat du jeu

---

## Commandes de build et execution

### Build
```bash
dune build                    # Compile tout le projet
dune build bin/newtonoid.exe  # Compile uniquement l'executable
dune clean                    # Nettoie les artefacts de build
```

### Execution
```bash
dune exec bin/newtonoid.exe   # Execute le jeu (commande officielle)
```

### Tests
```bash
dune runtest                  # Execute tous les tests
dune runtest lib              # Execute les tests de la bibliotheque uniquement
dune runtest -f               # Force re-execution des tests
```

### Tests individuels avec ppx_inline_test
Les tests sont definis avec `let%test` dans les fichiers `.ml` de `lib/`:
```ocaml
let%test "nom_du_test" = 
  (* expression booleenne *)
  ma_fonction arg = resultat_attendu
```

---

## Structure du projet

```
Newtonoid-PF/
├── bin/                 # Programme principal
│   ├── dune            # Configuration executable
│   └── newtonoid.ml    # Point d'entree du jeu
├── lib/                 # Bibliotheque libnewtonoid
│   ├── dune            # Configuration bibliotheque
│   ├── iterator.ml     # Module Flux (streams paresseux)
│   ├── game.ml         # Logique principale du jeu
│   ├── collisions.ml   # Detection et gestion des collisions
│   ├── brick.ml/.mli   # Type et operations sur les briques
│   ├── layout.ml/.mli  # Configuration des niveaux
│   └── input.ml/.mli   # Gestion des entrees souris
├── rapport/             # Rapport PDF du projet
├── dune-project         # Configuration dune
└── dune-workspace       # Workspace dune
```

### Organisation des modules

- **bin/** : Contient uniquement le programme principal. Peut utiliser `Libnewtonoid`.
- **lib/** : Contient tous les modules reutilisables. Peut utiliser `graphics`, `unix` et `ppx_inline_test`.

Pour utiliser les modules de la bibliotheque:
```ocaml
open Libnewtonoid
open Iterator
open Game
```

---

## Conventions de code

### Style general
- **Code majoritairement fonctionnel** (pas d'effets de bord sauf I/O)
- **Commentaires en francais**
- **Indentation: 2 espaces**

### Nommage
| Element | Convention | Exemple |
|---------|-----------|---------|
| Modules | PascalCase | `Iterator`, `Game`, `Brick` |
| Types | snake_case | `etat`, `brick`, `bricks` |
| Fonctions | snake_case | `rebond_boite`, `find_colliding_brick` |
| Variables | snake_case | `ball_pos`, `flux_vit` |
| Constantes module | PascalCase ou snake_case | `Box.marge`, `Init.dt` |

### Definition de types

Privilegier les records avec champs nommes:
```ocaml
(* BON : record avec champs explicites *)
type brick = {
  x      : float;          (* coin gauche *)
  y      : float;          (* coin bas *)
  width  : float;
  height : float;
  value  : int;            (* points *)
  color  : Graphics.color;
}

(* EVITER : alias simple *)
type t = int  (* Ce n'est pas une vraie definition de type ! *)
```

### Tuples pour les vecteurs
```ocaml
type vect = float * float  (* (x, y) *)

(* Usage *)
let (x, y) = position in
let (vx, vy) = velocity in
```

### Signatures de modules (.mli)

Toujours fournir une interface `.mli` pour les modules publics:
```ocaml
(* brick.mli *)
type brick = { ... }
type bricks = brick list
val remove_brick : brick -> bricks -> bricks
```

---

## Paradigme fonctionnel avec Flux

### Streams paresseux (Flux)

Le jeu utilise des flux paresseux pour l'evolution temporelle:
```ocaml
type 'a flux = Tick of ('a * 'a flux) option Lazy.t
```

### Operations principales sur les Flux
```ocaml
Flux.vide                    (* flux vide *)
Flux.cons x flux             (* ajoute element en tete *)
Flux.uncons flux             (* extrait tete et queue *)
Flux.map f flux              (* applique f a chaque element *)
Flux.map2 f flux1 flux2      (* combine deux flux *)
Flux.filter pred flux        (* filtre les elements *)
Flux.constant c              (* flux infini de c *)
Flux.unless flux cond f      (* branchement conditionnel *)
```

### Pattern d'integration (trajectoire)
```ocaml
let integre dt flux v0 =
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  let rec acc =
    Tick (lazy (Some (v0, Flux.map2 iter acc flux)))
  in acc
```

---

## Algorithmes requis

### Detection de collision Circle-AABB
```ocaml
(* Collision cercle (balle) avec rectangle (brique) *)
let circle_aabb_contact (cx, cy) radius bx by width height =
  let closest_x = max bx (min cx (bx +. width)) in
  let closest_y = max by (min cy (by +. height)) in
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in
  dx *. dx +. dy *. dy < radius *. radius
```

### Structure Quadtree (a implementer)
Pour une gestion efficace de l'espace de jeu, utiliser un quadtree:
```ocaml
type 'a quadtree =
  | Empty
  | Leaf of 'a list
  | Node of bounds * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
```

---

## Gestion des erreurs

- **Exception Graphics.Graphic_failure** : Ignoree (bug connu de Graphics)
- Utiliser `Option` pour les valeurs potentiellement absentes
- Pattern matching exhaustif obligatoire

```ocaml
match find_colliding_brick pos bricks radius with
| None -> (* pas de collision *)
| Some brick -> (* gerer la collision *)
```

---

## Parametres de jeu

Les parametres doivent etre facilement modifiables:
```ocaml
module Init = struct
  let dt = 1. /. 60.  (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx, infy = 10., 10.
  let supx, supy = 790., 590.
end

let g = 9.81 *. 50.  (* constante de gravite *)
let ball_radius = 10.0
```

---

## Checklist avant commit

1. `dune build` compile sans erreur
2. `dune runtest` passe tous les tests
3. Code fonctionnel (pas de mutation sauf I/O)
4. Types abstraits avec signatures .mli
5. Commentaires en francais
6. Pas de fichiers `_build/` commites

---

## Extensions possibles

- Animation de chute des briques
- Pouvoirs speciaux (acceleration, gravite, duplication balle)
- Changement de niveau
- Briques multi-hit ou indestructibles
- Gestion de la raquette avec impulsion

---

## Rappels importants

- Le code doit s'executer via `dune exec bin/newtonoid.exe`
- Utiliser les flux pour TOUTE evolution d'etat
- Collision: Circle-AABB obligatoire
- Espace de jeu: Quadtree ou equivalent obligatoire
- Modularite: parametres dynamiques facilement modifiables
