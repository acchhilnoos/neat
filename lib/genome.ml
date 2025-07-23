module Genome_Map = Map.Make (Int)

type 'a map = 'a Genome_Map.t

(*
    NOTE:
   - maybe Hashtbl
   - maybe connection innov array for O(1) random access
     - built only once, no mutation
*)
type t = {
  nodes : Node.t map;
  connections : Connection.t map;
  innovs : int array;
  fitness : float;
}

let empty =
  {
    nodes = Genome_Map.empty;
    connections = Genome_Map.empty;
    innovs = [||];
    fitness = 0.;
  }

let copy { nodes; connections; innovs; fitness } =
  { nodes; connections; innovs; fitness }

let add_node nd gn =
  { gn with nodes = Genome_Map.add (Node.get_id nd) nd gn.nodes }

let add_connection cn gn =
  let innov = Connection.get_innov cn in
  {
    gn with
    connections = Genome_Map.add innov cn gn.connections;
    innovs = Array.append gn.innovs [| innov |];
  }

let mutate_weights gn rs =
  let bound x = max (-1.) (min x 1.) in
  {
    gn with
    connections =
      Genome_Map.map
        (fun cn ->
          Connection.set_weight cn
            (if Random.State.int rs 10 < 9 then
               bound
                 (Connection.get_weight cn +. Random.State.float rs 0.2 -. 0.1)
             else Random.State.float rs 2. -. 1.))
        gn.connections;
  }

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
