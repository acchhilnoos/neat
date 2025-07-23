type t

val init : unit -> t
val cget : int -> int -> t -> int * t
val nget : t -> int * t
val rget_int : int -> t -> int * t
val rget_float : float -> t -> float * t
