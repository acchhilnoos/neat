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
  Genome_Tbl.add gn.nodes (Node.get_id nd) nd;
  gn

let update_node nd gn =
  let id = Node.get_id nd in
  Genome_Tbl.replace gn.nodes id nd;
  gn

let with_conn cn gn =
  let innov = Connection.get_innov cn in
  Genome_Tbl.add gn.connections innov cn;
  gn

let update_conn cn gn =
  let innov = Connection.get_innov cn in
  Genome_Tbl.replace gn.connections innov cn;
  gn

let init ?(bias = true) ic oc =
  let open Env.Let_syntax in
  let gn = empty () in
  let* i_ls =
    List.fold_left
      (fun acc _ ->
        let* ls = acc in
        let* id = Env.gen_id in
        let _ = with_node Node.(init id Input) gn in
        Env.return (id :: ls))
      (Env.return [])
      (List.init ic (fun _ -> 0))
  in
  let* ib_ls =
    if not bias then Env.return i_ls
    else
      let* id = Env.gen_id in
      let _ = with_node Node.(init id Bias) gn in
      Env.return (id :: i_ls)
  in
  let* o_ls =
    List.fold_left
      (fun acc _ ->
        let* ls = acc in
        let* id = Env.gen_id in
        let _ = with_node Node.(init id Output) gn in
        Env.return (id :: ls))
      (Env.return [])
      (List.init oc (fun _ -> 0))
  in
  List.fold_left
    (fun acc_i in_id ->
      acc_i
      >> List.fold_left
           (fun acc_j out_id ->
             let* innov = acc_j >> Env.gen_innov ~in_id ~out_id in
             let _ = with_conn (Connection.init in_id out_id innov) gn in
             Env.return ())
           (Env.return ()) o_ls)
    (Env.return ()) ib_ls
  >> Env.return gn

let update_layers (sn, sl) scs gn =
  let push v (i, o) = (v :: i, o)
  and pop (i, o) =
    if not (List.is_empty o) then List.(hd o, (i, tl o))
    else
      let o' = List.rev i in
      List.(hd o', ([], tl o'))
  and empty (i, o) = List.is_empty i && List.is_empty o in
  let rec bfs_update q =
    if empty q then gn
    else
      let (id, ly), q = pop q in
      let nd = Genome_Tbl.find gn.nodes id in
      let ly' = Node.get_layer nd in
      if ly' > ly || ly' = -1 then bfs_update q
      else
        let _ = update_node (Node.inc_layer nd) gn in
        let q' =
          List.fold_left
            (fun acc id -> push (id, ly + 1) acc)
            q (Genome_Tbl.find scs id)
        in
        bfs_update q'
  in
  ([], []) |> push (sn, sl) |> bfs_update

let make_scs gn =
  (* TODO: Hashtbl *)
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

let mutate_weights p gn =
  let open Env.Let_syntax in
  Genome_Tbl.fold
    (fun _ cn acc ->
      acc
      >>
      let* ri = Env.rand_float ~bound:1. in
      if ri < p then
        let* rf = Env.rand_float ~bound:0.2 in
        Env.return
          (update_conn
             (Connection.set_weight cn
                (max (-1.) (min 1. (Connection.get_weight cn +. rf -. 0.1))))
             gn)
      else
        let* rf = Env.rand_float ~bound:2. in
        Env.return
          (update_conn
             (Connection.set_weight cn (Connection.get_weight cn +. rf -. 1.))
             gn))
    gn.connections (Env.return gn)

let add_node scs gn =
  let open Env.Let_syntax in
  let* c_opt, _ =
    Genome_Tbl.fold
      (fun _ c_it acc ->
        if not (Connection.get_enabled c_it) then acc
        else
          let* copt_acc, i_acc = acc in
          let* ri = Env.rand_int ~bound:i_acc in
          if ri = 0 then Env.return (Some c_it, i_acc + 1)
          else Env.return (copt_acc, i_acc + 1))
      gn.connections
      (Env.return (None, 1))
  in
  match c_opt with
  | None -> Env.return gn
  | Some cn ->
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

let add_connection rand_its scs gn =
  let open Env.Let_syntax in
  let nds =
    Genome_Tbl.fold (fun id _ acc -> id :: acc) gn.nodes [] |> Array.of_list
  in
  let gn_has x y =
    match Genome_Tbl.find_opt scs x with
    | Some z -> List.mem y z
    | None -> false
  in
  let cond_do c a i o =
    if gn_has i o || gn_has o i then a ()
    else
      let il = Node.get_layer (Genome_Tbl.find gn.nodes i) in
      let ol = Node.get_layer (Genome_Tbl.find gn.nodes o) in
      if il = ol then a ()
      else if (il < ol && il <> -1) || ol = -1 then c i o
      else if (ol < il && ol <> -1) || il = -1 then c o i
      else a ()
  in
  let rec rand n =
    if n = 0 then Env.return None
    else
      let* i = Env.rand_int ~bound:(Array.length nds) in
      let* o = Env.rand_int ~bound:(Array.length nds) in
      let i = nds.(i) in
      let o = nds.(o) in
      cond_do (fun x y -> Env.return (Some (x, y))) (fun () -> rand (n - 1)) i o
  in
  let enum () =
    let free =
      Array.fold_left
        (fun acc i ->
          Array.fold_left
            (fun acc' o ->
              cond_do (fun x y -> (x, y) :: acc') (fun () -> acc') i o)
            acc nds)
        [] nds
      |> Array.of_list
    in
    if Array.length free = 0 then Env.return None
    else
      let* i = Env.rand_int ~bound:(Array.length free) in
      Env.return (Some free.(i))
  in
  let* pair = rand rand_its in
  match pair with
  | Some (x, y) ->
      let* iv = Env.gen_innov ~in_id:x ~out_id:y in
      Env.return (with_conn (Connection.init x y iv) gn)
  | None -> (
      let* pair = enum () in
      match pair with
      | Some (x, y) ->
          let* iv = Env.gen_innov ~in_id:x ~out_id:y in
          Env.return (with_conn (Connection.init x y iv) gn)
      | None -> Env.return gn)

let mutate ?(w = 0.8) ?(wp = 0.9) ?(nn = 0.03) ?(nc = 0.05) ?(ri = 10) gn =
  let open Env.Let_syntax in
  let* w' = Env.rand_float ~bound:1. in
  let* nn' = Env.rand_float ~bound:1. in
  let* nc' = Env.rand_float ~bound:1. in
  (if w' < w then mutate_weights wp else Env.return) gn
  >>= (if nn' < nn then fun gn' -> add_node (make_scs gn') gn' else Env.return)
  >>=
  if nc' < nc then fun gn' -> add_connection ri (make_scs gn') gn'
  else Env.return

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
