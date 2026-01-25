type t = { x : float; y : float }

(* Constructeurs *)
val create : float -> float -> t
val zero : t
val of_tuple : float * float -> t
val to_tuple : t -> float * float
