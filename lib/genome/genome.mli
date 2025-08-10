type t
(** genome type *)

val init : ?bias:bool -> int -> int -> t Env.t
val copy : t -> t
val add_node : int list Hashtbl.Make(Int).t -> t -> t Env.t
val add_connection : int -> int list Hashtbl.Make(Int).t-> t  -> t Env.t
val mutate_weights : float -> t -> t Env.t
val mutate : ?w:float -> ?wp:float -> ?nn:float -> ?nc:float -> ?ri:int -> t -> t Env.t
val pp : Format.formatter -> t -> unit
