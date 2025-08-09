module Genome_Tbl = Hashtbl.Make (Int)

type t = {
  nodes : Node.t Genome_Tbl.t;
  connections : Connection.t Genome_Tbl.t;
  fitness : float;
}

let empty () =
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

let with_node nd gn =
  let _ = Genome_Tbl.add gn.nodes (Node.get_id nd) nd in
  gn

let update_node nd gn =
  let id = Node.get_id nd in
  let _ = Genome_Tbl.replace gn.nodes id nd in
  gn

let with_conn cn gn =
  let innov = Connection.get_innov cn in
  let _ = Genome_Tbl.add gn.connections innov cn in
  gn

let update_conn cn gn =
  let innov = Connection.get_innov cn in
  let _ = Genome_Tbl.replace gn.connections innov cn in
  gn

let init ?(bias = true) ic oc =
  let open Env.Let_syntax in
  let ( >> ) = Env.( >> ) in
  let ( >>= ) = Env.( >>= ) in
  let gn = empty () in
  List.fold_left
    (fun acc _ ->
      acc >>= fun ls ->
      Env.gen_id >>= fun id ->
      let _ = with_node Node.(init id Input) gn in
      Env.return (id :: ls))
    (Env.return [])
    (List.init ic (fun _ -> 0))
  >>= fun i_ls ->
  (if bias then
     Env.gen_id >>= fun id ->
     let _ = with_node Node.(init id Bias) gn in
     Env.return (id :: i_ls)
   else Env.return i_ls)
  >>= fun ib_ls ->
  List.fold_left
    (fun acc _ ->
      acc >>= fun ls ->
      Env.gen_id >>= fun id ->
      let _ = with_node Node.(init id Output) gn in
      Env.return (id :: ls))
    (Env.return [])
    (List.init oc (fun _ -> 0))
  >>= fun o_ls ->
  List.fold_left
    (fun acc_i in_id ->
      acc_i
      >> List.fold_left
           (fun acc_j out_id ->
             acc_j >> Env.gen_innov ~in_id ~out_id >>= fun innov ->
             let _ = with_conn (Connection.init in_id out_id innov) gn in
             Env.return ())
           acc_i o_ls)
    (Env.return ()) ib_ls
  >> Env.return gn

(* let init ?(bias = true) ?(st = Env.init ()) ic oc = *)
(*   let gn = empty () in *)
(*   let i_ls, st = *)
(*     List.fold_left *)
(*       (fun (ls, st) _ -> *)
(*         let id, st' = Env.(run gen_id) st in *)
(*         let _ = with_node Node.(init id Input) gn in *)
(*         (id :: ls, st')) *)
(*       ([], st) *)
(*       (List.init ic (fun _ -> 0)) *)
(*   in *)
(*   let i_ls, st = *)
(*     if bias then *)
(*       let id, st' = Env.(run gen_id) st in *)
(*       let _ = with_node Node.(init id Bias) gn in *)
(*       (id :: i_ls, st') *)
(*     else (i_ls, st) *)
(*   in *)
(*   let o_ls, st = *)
(*     List.fold_left *)
(*       (fun (ls, st) _ -> *)
(*         let id, st' = Env.(run gen_id) st in *)
(*         let _ = with_node Node.(init id Output) gn in *)
(*         (id :: ls, st')) *)
(*       ([], st) *)
(*       (List.init oc (fun _ -> 0)) *)
(*   in *)
(*   let st = *)
(*     List.fold_left *)
(*       (fun st' i -> *)
(*         List.fold_left *)
(*           (fun st'' j -> *)
(*             let id, st''' = Env.(run (gen_innov ~in_id:i ~out_id:j)) st'' in *)
(*             let _ = with_conn (Connection.init i j id) gn in *)
(*             st''') *)
(*           st' o_ls) *)
(*       st i_ls *)
(*   in *)
(*   (gn, st) *)

let update_layers (sn, sl) scs gn =
  let push v (i, o) = (v :: i, o)
  and pop (i, o) =
    if List.is_empty o then
      let o' = List.rev i in
      List.(hd o', ([], tl o'))
    else List.(hd o, (i, tl o))
  and empty (i, o) = List.is_empty i && List.is_empty o in
  let rec bfs q =
    if empty q then gn
    else
      let (id, ly), q = pop q in
      let nd = Genome_Tbl.find gn.nodes id in
      let ly' = Node.get_layer nd in
      if ly' <= ly && ly' <> -1 then
        let _ = update_node (Node.inc_layer nd) gn in
        let q' =
          List.fold_left
            (fun acc id -> push (id, ly + 1) acc)
            q (Genome_Tbl.find scs id)
        in
        bfs q'
      else bfs q
  in
  ([], []) |> push (sn, sl) |> bfs

let make_scs gn =
  let scs = Genome_Tbl.create (Genome_Tbl.length gn.nodes) in
  let _ =
    Genome_Tbl.iter
      (fun _ cn ->
        let i = Connection.get_i_id cn in
        let o = Connection.get_o_id cn in
        Genome_Tbl.replace scs i
          (match Genome_Tbl.find_opt scs i with
          | Some x -> o :: x
          | None -> [ o ]))
      gn.connections
  in
  scs

let add_node gn =
  (* TODO: reservoir sampling *)
  let open Env.Let_syntax in
  let scs = make_scs gn in
  let cns =
    Genome_Tbl.fold
      (fun _ cn acc -> if Connection.get_enabled cn then cn :: acc else acc)
      gn.connections []
    |> Array.of_list
  in
  let* r = Env.rand_int ~bound:(Array.length cns) in
  let cn = cns.(r) in
  let ii = Connection.get_i_id cn in
  let oi = Connection.get_o_id cn in
  let* id = Env.gen_id in
  let il = Node.get_layer (Genome_Tbl.find gn.nodes ii) in
  let nd = Node.init id (Node.Hidden (il + 1)) in
  let* iv = Env.gen_innov ~in_id:ii ~out_id:id in
  let* ov = Env.gen_innov ~in_id:id ~out_id:oi in
  let ic = Connection.init ii id iv in
  let oc = Connection.init id oi ov ~weight:(Connection.get_weight cn) in
  gn |> with_node nd |> with_conn ic |> with_conn oc
  |> update_conn (Connection.disable cn)
  |> update_layers (oi, il + 1) scs
  |> Env.return

let add_connection gn =
  let open Env.Let_syntax in
  let nds =
    Genome_Tbl.fold (fun id _ acc -> id :: acc) gn.nodes [] |> Array.of_list
  in
  let scs = make_scs gn in
  let gn_has x y =
    match Genome_Tbl.find_opt scs x with
    | Some z -> List.mem y z
    | None -> false
  in
  let rec rand n =
    if n = 0 then Env.return None
    else
      let* i = Env.rand_int ~bound:(Array.length nds) in
      let i = nds.(i) in
      let* o = Env.rand_int ~bound:(Array.length nds) in
      let o = nds.(o) in
      let il = Node.get_layer (Genome_Tbl.find gn.nodes i) in
      let ol = Node.get_layer (Genome_Tbl.find gn.nodes o) in
      if il <> ol then
        if ((il < ol && il <> -1) || ol = -1) && not (gn_has i o) then
          Env.return (Some (i, o))
        else if ((ol < il && ol <> -1) || il = -1) && not (gn_has o i) then
          Env.return (Some (o, i))
        else rand (n - 1)
      else rand (n - 1)
  in
  let enum () =
    let free =
      Array.fold_left
        (fun acc i ->
          Array.fold_left
            (fun acc' o ->
              let il = Node.get_layer (Genome_Tbl.find gn.nodes i) in
              let ol = Node.get_layer (Genome_Tbl.find gn.nodes o) in
              if il <> ol then
                if ((il < ol && il <> -1) || ol = -1) && not (gn_has i o) then
                  (i, o) :: acc'
                else if ((ol < il && ol <> -1) || il = -1) && not (gn_has o i)
                then (o, i) :: acc'
                else acc'
              else acc')
            acc nds)
        [] nds
      |> Array.of_list
    in
    if Array.length free = 0 then Env.return None
    else
      let* i = Env.rand_int ~bound:(Array.length free) in
      Env.return (Some free.(i))
  in
  let* pair = rand 10 in
  match pair with
  | Some (x, y) ->
      let* iv = Env.gen_innov ~in_id:x ~out_id:y in
      Genome_Tbl.add gn.connections iv (Connection.init x y iv);
      Env.return gn
  | None -> (
      let* pair = enum () in
      match pair with
      | Some (x, y) ->
          let* iv = Env.gen_innov ~in_id:x ~out_id:y in
          Genome_Tbl.add gn.connections iv (Connection.init x y iv);
          Env.return gn
      | None -> Env.return gn)

let mutate_weights gn = Env.return gn

let pp_list ?(pp_sep = fun fmt () -> Format.fprintf fmt ";@   ") pp_item fmt xs
    =
  Format.pp_print_list ~pp_sep:(fun fmt () -> pp_sep fmt ()) pp_item fmt xs

let pp fmt gn =
  let nodes = Genome_Tbl.fold (fun _ n acc -> n :: acc) gn.nodes [] in
  let conns = Genome_Tbl.fold (fun _ c acc -> c :: acc) gn.connections [] in
  Format.fprintf fmt
    "@[<v 0>@[<v 8>nodes = [@;\
    \  %a;@;\
     ]@]@;\
     @[<v 8>conns = [@;\
    \  %a;@;\
     ]@]@;\
     fitness = %.3f@]@;"
    (pp_list Node.pp) nodes (pp_list Connection.pp) conns gn.fitness
