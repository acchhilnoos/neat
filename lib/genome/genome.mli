type t
(** genome type *)

val init : ?bias:bool -> int -> int -> t Env.t
val copy : t -> t
val add_node : t -> t Env.t
val add_connection : t -> t Env.t
val mutate_weights : t -> t Env.t
val pp : Format.formatter -> t -> unit
