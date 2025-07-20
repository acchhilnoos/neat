type kind = Input | Bias | Hidden | Output
type t

val reset_id : unit -> unit
val next_id : unit -> int
val init : kind -> t
val get_id : t -> int
val get_kind : t -> kind
val get_value : t -> float
