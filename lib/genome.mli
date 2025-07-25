type t
(** genome type *)

(* NOTE:
    - consider removing Random.State.t for perf increase
*)
val empty          : t
val copy           : t            -> t
val add_node       : Node.t       -> t         -> t
val add_connection : Connection.t -> t         -> t
val mutate_weights : t            -> Context.c -> t * Context.c
(* val mut_add_node   : t            -> t Context.t *)
(* val mut_add_connection : t            -> Context.t -> t * Context.t *)
