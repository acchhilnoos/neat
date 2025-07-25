module Genome_Map = Map.Make (Int)

type 'a map = 'a Genome_Map.t

module Vector : sig
  type t

  val init : unit -> t
  val get : int -> t -> int
  val size : t -> int
  val grow : t -> unit
  val append : int -> t -> unit
end = struct
  type t = {
    mutable data : int array;
    mutable size : int;
    mutable capacity : int;
  }

  let init () = { data = Array.init 32 (fun _ -> 0); size = 0; capacity = 32 }
  let get i v = v.data.(i)
  let size v = v.size

  let grow v =
    let capacity = v.capacity * 2 in
    let u = Array.init capacity (fun _ -> 0) in
    Array.blit v.data 0 u 0 v.capacity;
    v.data <- u;
    v.capacity <- capacity

  let append x v =
    if v.size = v.capacity then grow v;
    v.data.(v.size) <- x;
    v.size <- v.size + 1
end

(* TODO: Hashtbl *)
type t = {
  nodes : Node.t map;
  connections : Connection.t map;
  innovs : Vector.t;
  fitness : float;
}

let empty =
  {
    nodes = Genome_Map.empty;
    connections = Genome_Map.empty;
    innovs = Vector.init ();
    fitness = 0.;
  }

let copy { nodes; connections; innovs; fitness } =
  { nodes; connections; innovs; fitness }

let add_node nd gn =
  { gn with nodes = Genome_Map.add (Node.get_id nd) nd gn.nodes }

let add_connection cn gn =
  let innov = Connection.get_innov cn in
  Vector.append innov gn.innovs;
  { gn with connections = Genome_Map.add innov cn gn.connections }

let mutate_weights gn =
  let open Context in
  let open Context.Let_syntax in
  let bound x = max (-1.) (min x 1.)
  and connections = Genome_Map.bindings gn.connections in
  let rec mut_rec acc = function
    | [] -> return { gn with connections = Genome_Map.of_list (List.rev acc) }
    | (iv, cn) :: rest ->
        let* ri = rand_int 10 in
        let* w =
          if ri < 9 then
            let* rf = rand_float 0.2 in
            return (bound (Connection.get_weight cn +. rf -. 0.1))
          else
            let* rf = rand_float 2. in
            return (bound (rf -. 1.))
        in
        mut_rec ((iv, Connection.set_weight cn w) :: acc) rest
  in
  mut_rec [] connections

let mut_add_node gn =
  let open Context in
  let open Context.Let_syntax in
  let* ri = rand_int (Vector.size gn.innovs) in
  let innov = Vector.get ri gn.innovs in
  let cn = Genome_Map.find innov gn.connections in
  let cn = if Connection.get_enabled cn then Connection.toggle cn else cn in
  let i = Connection.get_i_id cn in
  let il = Node.get_layer (Genome_Map.find i gn.nodes) in
  let o = Connection.get_o_id cn in
  let* id = nget in
  let nd = Node.init id (Node.Hidden il) in
  let* ii = cget i id in
  let* oi = cget id o in
  let ic = Connection.init i id ii in
  let oc = Connection.init id o oi in
  let gn' =
    gn |> add_node nd |> add_connection ic |> add_connection oc
    |> add_connection cn
  in
  return gn'

let mut_add_connection gn ct = (gn, ct)
