type t
(** connection type *)

val init : ?enabled:bool -> ?weight:float -> int -> int -> int -> t
(** [init in out innov] creates a connection.

    [~enabled] and [~weight] allow the corresponding fields to be specified at creation (default=true,0.). *)

val get_i_id    : t -> int
val get_o_id    : t -> int
val get_innov   : t -> int
val get_enabled : t -> bool
val get_weight  : t -> float
val toggle      : t -> t
val set_weight  : t -> float -> t
val pp          : Format.formatter -> t -> unit
