type t

val empty : t
val copy : t -> t
val add_node : Node.t -> t -> t
val add_connection : Connection.t -> t -> t
val mutate_weights : t -> Random.State.t -> t
val mut_add_node : t -> Context.t -> t * Context.t
val mut_add_connection : t -> Context.t -> t * Context.t
