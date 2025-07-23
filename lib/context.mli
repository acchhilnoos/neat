type t
(** context type *)

val init  : unit              -> t
val cget  : int   -> int -> t -> int * t
val nget  : t                 -> int * t
val int   : int   -> t        -> int
val float : float -> t        -> float
