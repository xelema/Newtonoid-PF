type t = { x : float; y : float }

(* Constructeurs *)
val create : float -> float -> t
val zero : t
val of_tuple : float * float -> t
val to_tuple : t -> float * float

(* Operations arithmetiques *)
val add : t -> t -> t
val sub : t -> t -> t
val scale : float -> t -> t
val neg : t -> t

(* Operations geometriques *)
val dot : t -> t -> float
val magnitude : t -> float
val magnitude_squared : t -> float
val normalize : t -> t
val distance : t -> t -> float

(* Utilitaires *)
val clamp_magnitude : float -> t -> t
val lerp : float -> t -> t -> t
