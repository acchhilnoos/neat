type t = { genomes : Genome.t list; c_state : Cstate.t; n_state : Nstate.t }

let init ?(bias = true) i_count o_count g_count =
  let i_ids = List.init i_count (fun x -> x)
  and o_ids = List.init o_count (fun x -> x + i_count)
  and b_ids = [ i_count + o_count ]
  and cs = Cstate.empty
  and ns = Nstate.empty
  and gn = Genome.empty in
  let i_gn, i_ns =
    List.fold_left
      (fun (i_gn_acc, i_ns_acc) _ ->
        let i_id, i_ns' = Nstate.get i_ns_acc in
        let i_nd = Node.init i_id Node.Input in
        (Genome.add_node i_nd i_gn_acc, i_ns'))
      (gn, ns) i_ids
  in
  let o_gn, o_ns =
    List.fold_left
      (fun (o_gn_acc, o_ns_acc) _ ->
        let o_id, o_ns' = Nstate.get o_ns_acc in
        let o_nd = Node.init o_id Node.Output in
        (Genome.add_node o_nd o_gn_acc, o_ns'))
      (i_gn, i_ns) o_ids
  in
  let b_gn, n_state =
    if bias then
      let b_id, b_ns' = Nstate.get o_ns in
      let b_nd = Node.init b_id Node.Bias in
      (Genome.add_node b_nd o_gn, b_ns')
    else (o_gn, o_ns)
  in
  let c_gn, c_state =
    List.fold_left
      (fun (i_gn_acc, i_cs_acc) i_id ->
        List.fold_left
          (fun (o_gn_acc, o_cs_acc) o_id ->
            let innov, o_cs' = Cstate.get i_id o_id o_cs_acc in
            let cn = Connection.init i_id o_id innov in
            (Genome.add_connection cn o_gn_acc, o_cs'))
          (i_gn_acc, i_cs_acc) o_ids)
      (b_gn, cs) (i_ids @ b_ids)
  in
  { genomes = List.init g_count (fun _ -> Genome.copy c_gn); c_state; n_state }

let mutate gn = gn
