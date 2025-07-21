module Genome_Map = Map.Make (Int)

type 'a map = 'a Genome_Map.t
type t = { nodes : Node.t map; connections : Connection.t map; fitness : float }

let empty =
  { nodes = Genome_Map.empty; connections = Genome_Map.empty; fitness = 0. }

let add_node nd gn =
  { gn with nodes = Genome_Map.add (Node.get_id nd) nd gn.nodes }

let add_connection cn gn =
  {
    gn with
    connections = Genome_Map.add (Connection.get_innov cn) cn gn.connections;
  }

(*
let copy { nodes; connections; fitness } = { nodes; connections; fitness }

let add_node i t g =
  let n = Node.init i t in
  { g with nodes = Genome_Map.add (Node.get_id n) n g.nodes }

let add_connection i_id o_id g h =
  let i, h = History.innov i_id o_id h in
  ( {
      g with
      connections = Genome_Map.add i (Connection.init i_id o_id i) g.connections;
    },
    h )

let find_node n g = Genome_Map.find (Node.get_id n) g.nodes
let find_connection c g = Genome_Map.find (Connection.get_innov c) g.connections

let mutate_weights g =
  {
    g with
    connections =
      Genome_Map.map (fun c -> if Random.int 5 < 4 then c else c) g.connections;
  }

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
