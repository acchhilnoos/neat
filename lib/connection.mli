type t

val init : ?enabled: bool -> ?weight:float -> int -> int -> int -> t
val get_i_id : t -> int
val get_o_id : t -> int
val get_innov : t -> int
val get_enabled : t -> bool
val get_weight : t -> float
val toggle : t -> t
val set_weight : t -> float -> t
