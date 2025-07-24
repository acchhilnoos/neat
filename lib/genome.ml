module Genome_Map = Map.Make (Int)

type 'a map = 'a Genome_Map.t

module Vector = struct
  type t = {
    mutable data : int array;
    mutable size : int;
    mutable capacity : int;
  }

  let init () = { data = Array.init 32 (fun _ -> 0); size = 0; capacity = 32 }
  let get i v = v.data.(i)

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

(*
    NOTE:
   - maybe Hashtbl
*)
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
            return (rf -. 1.)
        in
        mut_rec ((iv, Connection.set_weight cn w) :: acc) rest
  in
  mut_rec [] connections

(* ( { *)
(*     gn with *)
(*     connections = *)
(*       Genome_Map.map *)
(*         (fun cn -> *)
(*           let w, _ = *)
(*             Context.( *)
(*               run *)
(*                 ( rand_int 10 >>= fun ri -> *)
(*                   if ri < 9 then *)
(*                     rand_float 0.2 >>= fun rf -> *)
(*                     return (bound (Connection.get_weight cn +. rf -. 0.1)) *)
(*                   else rand_float 2.0 >>= fun rf -> return rf ) *)
(*                 ct) *)
(*           in *)
(*           Connection.set_weight cn w) *)
(*         gn.connections; *)
(*   }, *)
(*   ct ) *)

let mut_add_node gn ct = (gn, ct)
let mut_add_connection gn ct = (gn, ct)

(*
let mut_add_node g h =
  let r = Random.int (Genome_Map.cardinal g.connections) in
  let innov, c, _ =
    Genome_Map.fold
      (fun i c (pi, pc, n) -> if n = r then (i, c, n + 1) else (pi, pc, n + 1))
      g.connections
      (0, snd (Genome_Map.choose g.connections), 0)
  in
  let n = Node.init Node.Hidden in
  let id = Node.get_id n in
  let nodes = Genome_Map.add id n g.nodes in
  let i_id = Connection.get_i_id c in
  let o_id = Connection.get_o_id c in
  let i_innov, h = History.innov i_id id h in
  let o_innov, h = History.innov id o_id h in
  let connections =
    g.connections
    |> Genome_Map.add innov (Connection.toggle c)
    |> Genome_Map.add i_innov (Connection.init i_id id i_innov)
    |> Genome_Map.add o_innov
         (Connection.init ~weight:(Connection.get_weight c) id o_id o_innov)
  in
  ({ g with nodes; connections }, h)

let mut_add_conn g = g
*)
