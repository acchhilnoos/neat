type t = { genomes : Genome.t list; c_state : Cstate.t; n_state : Nstate.t }

let init ?(bias = true) i_count o_count g_count =
  let cs = Cstate.empty and ns = Nstate.empty and gn = Genome.empty in
  let i_gn, i_ns =
    List.fold_left
      (fun (i_gn_acc, i_ns_acc) _ ->
        let i_id, i_ns' = Nstate.get i_ns_acc in
        let i_nd = Node.init i_id Node.Input in
        (Genome.add_node i_nd i_gn_acc, i_ns'))
      (gn, ns)
      (List.init i_count (fun _ -> ()))
  in
  let b_gn, b_ns =
    if bias then
      let b_id, b_ns' = Nstate.get i_ns in
      let b_nd = Node.init b_id Node.Bias in
      (Genome.add_node b_nd i_gn, b_ns')
    else (i_gn, i_ns)
  in
  let o_gn, n_state =
    List.fold_left
      (fun (o_gn_acc, o_ns_acc) _ ->
        let o_id, o_ns' = Nstate.get o_ns_acc in
        let o_nd = Node.init o_id Node.Output in
        (Genome.add_node o_nd o_gn_acc, o_ns'))
      (b_gn, b_ns)
      (List.init o_count (fun _ -> ()))
  in
  let c_gn, c_state =
    List.fold_left
      (fun (i_gn_acc, i_cs_acc) i_id ->
        List.fold_left
          (fun (o_gn_acc, o_cs_acc) o_id ->
            let innov, o_cs' = Cstate.get i_id o_id o_cs_acc in
            let cn = Connection.init i_id o_id innov in
            (Genome.add_connection cn o_gn_acc, o_cs'))
          (i_gn_acc, i_cs_acc)
          (List.init o_count (fun y -> y + (i_count + if bias then 1 else 0))))
      (o_gn, cs)
      (List.init (i_count + if bias then 1 else 0) (fun x -> x))
  in
  { genomes = List.init g_count (fun _ -> c_gn); c_state; n_state }

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
