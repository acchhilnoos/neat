type t = { genomes : Genome.t list; c_state : Cstate.t; nstate : Nstate.t }

let init ?(bias = true) i_count o_count g_count =
  let cs = Cstate.empty
  and ns = Nstate.empty
  and gn = Genome.empty in
  0

(*
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
*)
