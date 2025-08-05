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

let an nd gn =
  let _ = Genome_Tbl.add gn.nodes (Node.get_id nd) nd in
  gn

let un nd gn =
  let id = Node.get_id nd in
  let _ = Genome_Tbl.replace gn.nodes id nd in
  gn

let ac cn gn =
  let innov = Connection.get_innov cn in
  let _ = Genome_Tbl.add gn.connections innov cn in
  gn

let uc cn gn =
  let innov = Connection.get_innov cn in
  let _ = Genome_Tbl.replace gn.connections innov cn in
  gn

let init ?(bias = true) ic oc =
  let open Context in
  let st = Context.init () in
  let gn = empty in
  let i_ls, st =
    List.fold_left
      (fun (ls, st) _ ->
        let id, st' = run nget st in
        let _ = an (Node.init id Node.Input) gn in
        (id :: ls, st'))
      ([], st)
      (List.init ic (fun _ -> 0))
  in
  let i_ls, st =
    if bias then
      let id, st' = run nget st in
      let _ = an (Node.init id Node.Bias) gn in
      (id :: i_ls, st')
    else (i_ls, st)
  in
  let o_ls, st =
    List.fold_left
      (fun (ls, st) _ ->
        let id, st' = run nget st in
        let _ = an (Node.init id Node.Output) gn in
        (id :: ls, st'))
      ([], st)
      (List.init oc (fun _ -> 0))
  in
  let st =
    List.fold_left
      (fun st' i ->
        List.fold_left
          (fun st'' j ->
            let id, st''' = run (cget i j) st'' in
            let _ = ac (Connection.init i j id) gn in
            st''')
          st' o_ls)
      st i_ls
  in
  (gn, st)

let update_layers (sn, sl) scs gn =
  let push v (o, i) = (o, v :: i)
  and pop (o, i) =
    if List.is_empty o then
      let o' = List.rev i in
      (List.hd o', (List.tl o', []))
    else (List.hd o, (List.tl o, i))
  and empty (o, i) = List.is_empty o && List.is_empty i
  and q = ([ (sn, sl) ], []) in
  let rec bfs q =
    if empty q then gn
    else
      let (id, ly), q = pop q in
      let nd = Genome_Tbl.find gn.nodes id in
      if Node.get_layer nd <= ly then
        let _ = un (Node.inc_layer nd) gn in
        let q' =
          List.fold_left
            (fun q'' id -> push (id, ly + 1) q'')
            q
            (Genome_Tbl.find scs (Node.get_id nd))
        in
        bfs q'
      else bfs q
  in
  bfs q

let make_scs gn =
  let scs = Genome_Tbl.create (Genome_Tbl.length gn.nodes) in
  let _ =
    Genome_Tbl.iter
      (fun _ cn ->
        let i = Connection.get_i_id cn in
        Genome_Tbl.replace scs i
          (match Genome_Tbl.find_opt scs i with
          | Some x -> Connection.get_o_id cn :: x
          | None -> [ Connection.get_o_id cn ]))
      gn.connections
  in
  scs

let add_node gn =
  (* TODO: reservoir sampling *)
  let open Context in
  let open Let_syntax in
  let scs = make_scs gn in
  let cns =
    Genome_Tbl.fold (fun _ cn acc -> cn :: acc) gn.connections []
    |> Array.of_list
  in
  let* r = m_rand_int (Array.length cns) in
  let cn = cns.(r) in
  let ii = Connection.get_i_id cn in
  let oi = Connection.get_o_id cn in
  let* id = nget in
  let il = Node.get_layer (Genome_Tbl.find gn.nodes ii) in
  let nd = Node.init id (Node.Hidden (il + 1)) in
  let* iv = cget ii id in
  let* ov = cget id oi in
  let ic = Connection.init ii id iv in
  let oc = Connection.init id oi ov ~weight:(Connection.get_weight cn) in
  gn |> an nd |> ac ic |> ac oc
  |> uc (Connection.toggle cn)
  |> update_layers (id, il + 1) scs
  |> return

let add_connection gn =
  let open Context in
  let open Let_syntax in
  let nds =
    Genome_Tbl.fold (fun id _ acc -> id :: acc) gn.nodes [] |> Array.of_list
  in
  let scs = make_scs gn in
  let rec exists x y =
    match Genome_Tbl.find_opt scs x with
    | Some z -> List.mem y z
    | None -> false
  in
  let rec rand n =
    if n = 0 then return None
    else
      let* i = m_rand_int (Array.length nds) in
      let i = nds.(i) in
      let* o = m_rand_int (Array.length nds) in
      let o = nds.(o) in
      let i, o =
        if
          Node.get_layer (Genome_Tbl.find gn.nodes i)
          < Node.get_layer (Genome_Tbl.find gn.nodes o)
        then (i, o)
        else (o, i)
      in
      if exists i o then rand (n - 1) else return (Some (i, o))
  in
  let enum () =
    let free =
      Array.fold_left
        (fun acc i ->
          Array.fold_left
            (fun acc' o ->
              let il = Node.get_layer (Genome_Tbl.find gn.nodes i) in
              let ol = Node.get_layer (Genome_Tbl.find gn.nodes o) in
              if il < ol && not (exists i o) then (i, o) :: acc' else acc)
            acc nds)
        [] nds
      |> Array.of_list
    in
    let* i = m_rand_int (Array.length free) in
    return free.(i)
  in
  let* rpair = rand 10 in
  match rpair with Some x -> return x | None -> enum ()

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

let pp_list ?(pp_sep = fun fmt () -> Format.fprintf fmt ";@ ") pp_item fmt xs =
  Format.pp_print_list ~pp_sep:(fun fmt () -> pp_sep fmt ()) pp_item fmt xs

let pp fmt gn =
  let nodes = Genome_Tbl.fold (fun _ n acc -> n :: acc) gn.nodes [] in
  let conns = Genome_Tbl.fold (fun _ c acc -> c :: acc) gn.connections [] in
  Format.fprintf fmt
    "@[<v 2>{@@[<v 2>nodes=[@;\
     <1 2>%a@]@]@@[<v 2>conns=[@;\
     <1 2>%a@]@]@fitness=%.3f@@]}" (pp_list Node.pp) nodes
    (pp_list Connection.pp) conns gn.fitness
