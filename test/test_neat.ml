(* context *)

let st = ref (Env.init ())

let get_id () =
  let i, st' = Env.run Env.gen_id !st in
  st := st';
  i

let get_iv i o =
  let i, st' = Env.run (Env.gen_innov ~in_id:i ~out_id:o) !st in
  st := st';
  i

let get_int b =
  let i, st' = Env.run (Env.rand_int ~bound:b) !st in
  st := st';
  i

let get_float b =
  let i, st' = Env.run (Env.rand_float ~bound:b) !st in
  st := st';
  i

(* context *)

(* connection *)

let c_iid () =
  let i = get_int 100 in
  let o = get_int 100 in
  let v = get_iv i o in
  let cn = Connection.init i o v in
  (* Format.printf "c_iid     conn : %a@." Connection.pp cn; *)
  Alcotest.(check int) "in id" i (Connection.get_i_id cn)

let c_oid () =
  let i = get_int 100 in
  let o = get_int 100 in
  let v = get_iv i o in
  let cn = Connection.init i o v in
  (* Format.printf "c_oid     conn : %a@." Connection.pp cn; *)
  Alcotest.(check int) "out id" o (Connection.get_o_id cn)

let c_innov () =
  let i = get_int 100 in
  let o = get_int 100 in
  let v = get_iv i o in
  let cn = Connection.init i o v in
  (* Format.printf "c_innov   conn : %a@." Connection.pp cn; *)
  Alcotest.(check int) "innov" v (Connection.get_innov cn)

let c_enabled () =
  let i1 = get_int 100 in
  let o1 = get_int 100 in
  let v1 = get_iv i1 o1 in
  let i2 = get_int 100 in
  let o2 = get_int 100 in
  let v2 = get_iv i2 o2 in
  let cn1 = Connection.init i1 o1 v1 in
  let cn2 = Connection.init ~enabled:false i2 o2 v2 in
  (* Format.printf "c_enabled conn1: %a@." Connection.pp cn1; *)
  (* Format.printf "c_enabled conn2: %a@." Connection.pp cn2; *)
  (* Format.printf "c_toggled conn2: %a@." Connection.pp (Connection.toggle cn2); *)
  Alcotest.(check bool) "default" true (Connection.get_enabled cn1);
  Alcotest.(check bool)
    "toggle disabled" false
    (Connection.get_enabled (Connection.disable cn2));
  Alcotest.(check bool) "disabled" false (Connection.get_enabled cn2);
  Alcotest.(check bool)
    "toggle enabled" true
    (Connection.get_enabled (Connection.enable cn2))

let c_weight () =
  let i1 = get_int 100 in
  let o1 = get_int 100 in
  let v1 = get_iv i1 o1 in
  let i2 = get_int 100 in
  let o2 = get_int 100 in
  let v2 = get_iv i2 o2 in
  let w1 = get_float 2. -. 1. in
  let w2 = get_float 2. -. 1. in
  let cn1 = Connection.init i1 o1 v1 in
  let cn2 = Connection.init ~weight:w1 i2 o2 v2 in
  (* Format.printf "c_weight  conn1: %a@." Connection.pp cn1; *)
  (* Format.printf "c_weight  conn2: %a@." Connection.pp cn2; *)
  (* Format.printf "c_set     conn2: %a@." Connection.pp *)
  (* (Connection.set_weight cn2 w2); *)
  Alcotest.(check (float 0.)) "default" 1. (Connection.get_weight cn1);
  Alcotest.(check (float 0.)) "w" w1 (Connection.get_weight cn2);
  Alcotest.(check (float 0.))
    "w2" w2
    (Connection.get_weight (Connection.set_weight cn2 w2))

(* connection *)

(* node *)

let n_id () =
  let id1 = get_id () in
  let id2 = get_id () in
  let id3 = get_id () in
  let id4 = get_id () in
  let ly = get_int 100 in
  let nd1 = Node.init id1 Node.Input in
  let nd2 = Node.init id2 Node.Bias in
  let nd3 = Node.init id3 (Node.Hidden ly) in
  let nd4 = Node.init id4 Node.Output in
  (* Format.printf "nd1: %a@." Node.pp nd1; *)
  (* Format.printf "nd2: %a@." Node.pp nd2; *)
  (* Format.printf "nd3: %a@." Node.pp nd3; *)
  (* Format.printf "nd4: %a@." Node.pp nd4; *)
  Alcotest.(check int) "id1" id1 (Node.get_id nd1);
  Alcotest.(check int) "id2" id2 (Node.get_id nd2);
  Alcotest.(check int) "id3" id3 (Node.get_id nd3);
  Alcotest.(check int) "id4" id4 (Node.get_id nd4)

let n_ly () =
  let id1 = get_id () in
  let id2 = get_id () in
  let id3 = get_id () in
  let id4 = get_id () in
  let ly = get_int 100 in
  let nd1 = Node.init id1 Node.Input in
  let nd2 = Node.init id2 Node.Bias in
  let nd3 = Node.init id3 (Node.Hidden ly) in
  let nd4 = Node.init id4 Node.Output in
  (* Format.printf "nd1: %a@." Node.pp nd1; *)
  (* Format.printf "nd2: %a@." Node.pp nd2; *)
  (* Format.printf "nd3: %a@." Node.pp nd3; *)
  (* Format.printf "inc: %a@." Node.pp (Node.inc_layer nd3); *)
  (* Format.printf "nd4: %a@." Node.pp nd4; *)
  Alcotest.(check int) "ly1" 0 (Node.get_layer nd1);
  Alcotest.(check int) "ly2" 0 (Node.get_layer nd2);
  Alcotest.(check int) "ly3" ly (Node.get_layer nd3);
  Alcotest.(check int) "inc" (1 + ly) (Node.get_layer (Node.inc_layer nd3));
  Alcotest.(check int) "ly4" (-1) (Node.get_layer nd4)

let n_val () =
  let id1 = get_id () in
  let id2 = get_id () in
  let id3 = get_id () in
  let id4 = get_id () in
  let ly = get_int 100 in
  let val1 = get_float 5. -. 2.5 in
  let val2 = get_float 5. -. 2.5 in
  let val3 = get_float 5. -. 2.5 in
  let val4 = get_float 5. -. 2.5 in
  let val5 = get_float 5. -. 2.5 in
  let nd1 = Node.init ~value:val1 id1 Node.Input in
  let nd2 = Node.init ~value:val2 id2 Node.Bias in
  let nd3 = Node.init ~value:val3 id3 (Node.Hidden ly) in
  let nd4 = Node.init ~value:val4 id4 Node.Output in
  (* Format.printf "nd1: %a@." Node.pp nd1; *)
  (* Format.printf "nd2: %a@." Node.pp nd2; *)
  (* Format.printf "nd3: %a@." Node.pp nd3; *)
  (* Format.printf "nd4: %a@." Node.pp nd4; *)
  (* Format.printf "set: %a@." Node.pp (Node.set_value nd4 val5); *)
  Alcotest.(check (float 0.)) "val1" val1 (Node.get_value nd1);
  Alcotest.(check (float 0.)) "val2" val2 (Node.get_value nd2);
  Alcotest.(check (float 0.)) "val3" val3 (Node.get_value nd3);
  Alcotest.(check (float 0.)) "val4" val4 (Node.get_value nd4);
  Alcotest.(check (float 0.))
    "val5" val5
    (Node.get_value (Node.set_value nd4 val5))

(* node *)

(* genome *)

let g_init () =
  let open Env.Let_syntax in
  let _ =
    Env.run
      (let* gn1 = Genome.init 3 3 in
       Env.return (Format.printf "gn1:%a@." Genome.pp gn1))
      !st
  in
  ();
  Alcotest.(check bool) "init" true true

(* genome *)

(* Run it *)
let () =
  let open Alcotest in
  run ~and_exit:true ~verbose:true "Primitives"
    [
      ( "connection",
        [ (* test_case "in id" `Quick c_iid; *)
          (* test_case "out id" `Quick c_oid; *)
          (* test_case "innov" `Quick c_innov; *)
          (* test_case "enabled" `Quick c_enabled; *)
          (* test_case "weight" `Quick c_weight; *) ] );
      ( "node",
        [ (* test_case "id" `Quick n_id; *)
          (* test_case "layer" `Quick n_ly; *)
          (* test_case "value" `Quick n_val; *) ] );
      ( "genome",
        [
          test_case "init" `Quick g_init;
          (* test_case "add node, update layers" `Quick g_add_node; *)
          (* test_case "add connection" `Quick g_add_conn; *)
          (* test_case "mutate weights" `Quick g_mut_weights; *)
        ] );
    ]
