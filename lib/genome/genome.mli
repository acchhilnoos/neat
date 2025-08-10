type t
(** genome type *)

val init : ?bias:bool -> int -> int -> t Env.t
val copy : t -> t
val add_node : t -> int list Hashtbl.Make(Int).t -> t Env.t
val add_connection : ?rand_its:int -> t -> int list Hashtbl.Make(Int).t -> t Env.t
val mutate_weights : t -> t Env.t
val pp : Format.formatter -> t -> unit
