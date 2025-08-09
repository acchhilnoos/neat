type c

type 'a t
(** context type *)

val init : unit -> c
val return : 'a -> 'a t
val get : c t
val set : c -> unit t
val ( >> ) : 'a t -> 'b t -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val run : 'a t -> c -> 'a * c
val gen_innov : in_id:int -> out_id:int -> int t
val gen_id : int t
val rand_int : bound:int -> int t
val rand_float : bound:float -> float t

module Let_syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
