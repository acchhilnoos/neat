type t

val empty : t
val copy : t -> t
val add_node : Node.t -> t -> t
val add_connection : Connection.t -> t -> t
