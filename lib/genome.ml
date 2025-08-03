module Vector : sig
  type t

  val init : unit -> t
  val get : int -> t -> int
  val copy : t -> t
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
  let copy { data; size; capacity } = { data = Array.copy data; size; capacity }

  let grow v =
    let capacity = Int.of_float (Float.ceil (Int.to_float v.capacity *. 2.)) in
    let u = Array.init capacity (fun _ -> 0) in
    Array.blit v.data 0 u 0 v.capacity;
    v.data <- u;
    v.capacity <- capacity

  let append x v =
    if v.size = v.capacity then grow v;
    v.data.(v.size) <- x;
    v.size <- v.size + 1
end

module Genome_Tbl = Hashtbl.Make (Int)

type t = {
  nodes : Node.t Genome_Tbl.t; (* id -> node *)
  connections : Connection.t Genome_Tbl.t; (* innov -> connection *)
  fitness : float;
}

let empty =
  {
    nodes = Genome_Tbl.create 32;
    connections = Genome_Tbl.create 32;
    fitness = 0.;
  }

let copy { nodes; connections; fitness } =
  {
    nodes = Genome_Tbl.copy nodes;
    connections = Genome_Tbl.copy connections;
    fitness;
  }

let add_node nd gn =
  let _ = Genome_Tbl.add gn.nodes (Node.get_id nd) nd in
  gn

let add_connection cn gn =
  Connection.(
    let i = get_i_id cn and o = get_o_id cn and innov = get_innov cn in
    let _ = Genome_Tbl.add gn.connections innov cn in
    gn)

let init ?(bias = true) i_count o_count st =
  let gn = empty in
  let i_ls, st =
    List.fold_left
      (fun (ls, st) _ ->
        let id, st' = Context.run Context.nget st in
        let _ = add_node (Node.init id Node.Input) gn in
        (id :: ls, st'))
      ([], st)
      (List.init i_count (fun _ -> 0))
  in
  let i_ls, st =
    if bias then
      let id, st' = Context.run Context.nget st in
      let _ = add_node (Node.init id Node.Bias) gn in
      (id :: i_ls, st')
    else (i_ls, st)
  in
  let o_ls, st =
    List.fold_left
      (fun (ls, st) _ ->
        let id, st' = Context.run Context.nget st in
        let _ = add_node (Node.init id Node.Output) gn in
        (id :: ls, st'))
      ([], st)
      (List.init o_count (fun _ -> 0))
  in
  let st =
    List.fold_left
      (fun st' i ->
        List.fold_left
          (fun st'' j ->
            let id, st''' = Context.run (Context.cget i j) st'' in
            let _ = add_connection (Connection.init i j id) gn in
            st''')
          st' o_ls)
      st i_ls
  in
  gn, st

(* let update_layers id ly gn = *)
(*   let rec update_rec cur nxt l = *)
(*     match cur with *)
(*     | x :: xs -> *)
(*         if Node.get_layer (Genome_Tbl.find gn.nodes x) <= l then *)
(*           let _ = *)
(*             Genome_Tbl.replace gn.nodes x *)
(*               (Node.inc_layer (Genome_Tbl.find gn.nodes x)) *)
(*           in *)
(*           update_rec xs *)
(*             (match Genome_Tbl.find_opt gn.succs x with *)
(*             | Some x -> x @ nxt *)
(*             | None -> nxt) *)
(*             l *)
(*         else update_rec xs nxt l *)
(*     | [] -> if nxt = [] then gn else update_rec nxt [] (l + 1) *)
(*   in *)
(*   match Genome_Tbl.find_opt gn.succs id with *)
(*   | Some x -> update_rec x [] ly *)
(*   | None -> gn *)

(* let mutate_weights gn = *)
(*   let open Context in *)
(*   let open Let_syntax in *)
(*   let bound x = max (-1.) (min x 1.) *)
(*   and cns = *)
(*     Genome_Tbl.fold (fun innov cn acc -> (innov, cn) :: acc) gn.connections [] *)
(*   in *)
(*   let rec mut_rec = function *)
(*     | [] -> return gn *)
(*     | (innov, cn) :: rest -> *)
(*         let* ri = rand_int 10 in *)
(*         let* w = *)
(*           if ri < 9 then *)
(*             let* rf = rand_float 0.2 in *)
(*             return (bound (Connection.get_weight cn +. rf -. 0.1)) *)
(*           else *)
(*             let* rf = rand_float 2. in *)
(*             return (rf -. 1.) *)
(*         in *)
(*         let cn' = Connection.set_weight cn w in *)
(*         let _ = Genome_Tbl.replace gn.connections innov cn' in *)
(*         mut_rec rest *)
(*   in *)
(*   mut_rec cns *)

(* let mut_add_node gn = *)
(*   let open Context in *)
(*   let open Let_syntax in *)
(*   let* ri = rand_int (Vector.size gn.innovs) in *)
(*   let innov = Vector.get ri gn.innovs in *)
(*   let cn = Genome_Tbl.find gn.connections innov in *)
(*   let cn = if Connection.get_enabled cn then Connection.toggle cn else cn in *)
(*   let i = Connection.get_i_id cn in *)
(*   let il = Node.get_layer (Genome_Tbl.find gn.nodes i) in *)
(*   let o = Connection.get_o_id cn in *)
(*   let* id = nget in *)
(*   let nd = Node.init id (Node.Hidden (il + 1)) in *)
(*   let* ii = cget i id in *)
(*   let* oi = cget id o in *)
(*   let ic = Connection.init i id ii in *)
(*   let oc = Connection.init id o oi in *)
(*   let gn' = *)
(*     gn |> add_node nd |> add_connection ic |> add_connection oc *)
(*     |> add_connection cn *)
(*     |> update_layers id (il + 1) *)
(*   in *)
(*   return gn' *)

let mut_add_connection gn ct = (gn, ct)
