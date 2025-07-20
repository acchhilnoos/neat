module History = struct
  module History_Map = Map.Make (struct
    type t = int * int

    let compare (a, b) (c, d) =
      match Int.compare a c with 0 -> Int.compare b d | x -> x
  end)

  type t = { map : int History_Map.t; counter : int }

  let empty = { map = History_Map.empty; counter = 0 }

  let innov i_id o_id { map; counter } =
    match History_Map.find_opt (i_id, o_id) map with
    | Some i -> (i, { map; counter })
    | None ->
        let i = counter + 1 in
        (i, { map = History_Map.add (i_id, o_id) i map; counter = i })
end

module Genome = struct
  module Genome_Map = Map.Make (Int)

  type 'a map = 'a Genome_Map.t

  type t = {
    nodes : Node.t map;
    connections : Connection.t map;
    fitness : float;
  }

  let empty =
    { nodes = Genome_Map.empty; connections = Genome_Map.empty; fitness = 0. }

  let copy { nodes; connections; fitness } = { nodes; connections; fitness }

  let add_node t g =
    let n = Node.init t in
    { g with nodes = Genome_Map.add (Node.get_id n) n g.nodes }

  let add_connection i_id o_id g h =
    let i, h = History.innov i_id o_id h in
    ( {
        g with
        connections =
          Genome_Map.add i (Connection.init i_id o_id i) g.connections;
      },
      h )

  let find_node n g = Genome_Map.find (Node.get_id n) g.nodes

  let find_connection c g =
    Genome_Map.find (Connection.get_innov c) g.connections

  let mutate_weights g =
    {
      g with
      connections =
        Genome_Map.map
          (fun c -> if Random.int 5 < 4 then c else c)
          g.connections;
    }

  let mut_add_node g h =
    let r = Random.int (Genome_Map.cardinal g.connections) in
    let innov, c, _ =
      Genome_Map.fold
        (fun i c (pi, pc, n) ->
          if n = r then (i, c, n + 1) else (pi, pc, n + 1))
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
end

type t = { genomes : Genome.t list; history : History.t }

let init ?(bias = true) i_count o_count g_count =
  let i_ids = List.init i_count (fun x -> x) in
  let o_ids = List.init o_count (fun x -> x + i_count) in
  let gi =
    List.fold_left
      (fun gi' _ -> Genome.add_node Node.Input gi')
      Genome.empty i_ids
  in
  let go =
    List.fold_left (fun go' _ -> Genome.add_node Node.Output go') gi o_ids
  in
  let gb = if bias then Genome.add_node Node.Bias go else go in
  let gn, history =
    List.fold_left (* for each input (and bias) node id *)
      (fun (gn', h) i_id ->
        List.fold_left (* for each output node id *)
          (fun (gn'', h') o_id ->
            (* generate an innov, initialize, and add a connection *)
            Genome.add_connection i_id o_id gn'' h')
          (gn', h) o_ids)
      (gb, History.empty)
      (i_ids @ if bias then [ i_count + o_count ] else [])
  in
  { genomes = List.init g_count (fun _ -> Genome.copy gn); history }

let mutate g h =
  Genome.(
    let g = if Random.int 5 < 4 then mutate_weights g else g in
    let g, h = if Random.int 100 < 3 then mut_add_node g h else (g, h) in
    let g = if Random.int 20 < 1 then mut_add_conn g else g in
    g)
