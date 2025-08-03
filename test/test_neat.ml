let c_iid () =
  let open Neat in
  let cn = Connection.init 0 1 2 in
  Alcotest.(check int) "in id" 0 (Connection.get_i_id cn)

let c_oid () =
  let open Neat in
  let cn = Connection.init 0 1 2 in
  Alcotest.(check int) "out id" 1 (Connection.get_o_id cn)

let c_innov () =
  let open Neat in
  let cn = Connection.init 0 1 2 in
  Alcotest.(check int) "innov" 2 (Connection.get_innov cn)

let c_enabled () =
  let open Neat in
  let cn1 = Connection.init 0 1 0 in
  let cn2 = Connection.init ~enabled:false 0 1 0 in
  Alcotest.(check bool) "default" true (Connection.get_enabled cn1);
  Alcotest.(check bool) "disabled" false (Connection.get_enabled cn2);
  Alcotest.(check bool)
    "toggle enabled" true
    (Connection.get_enabled (Connection.toggle cn2))

let c_weight () =
  let open Neat in
  let cn1 = Connection.init 0 1 0 in
  let cn2 = Connection.init ~weight:0.2 0 1 0 in
  Alcotest.(check (float 0.)) "default" 1. (Connection.get_weight cn1);
  Alcotest.(check (float 0.)) "0.2" 0.2 (Connection.get_weight cn2);
  Alcotest.(check (float 0.))
    "toggle 0.5" 0.5
    (Connection.get_weight (Connection.set_weight cn2 0.5))

(* Run it *)
let () =
  let open Alcotest in
  run "Primitives"
    [
      ( "connection",
        [
          test_case "in id" `Quick c_iid;
          test_case "out id" `Quick c_oid;
          test_case "innov" `Quick c_innov;
          test_case "enabled" `Quick c_enabled;
          test_case "weight" `Quick c_weight;
        ] );
      (* "string-concat", [ test_case "String mashing" `Quick test_str_concat  ]; *)
      (* "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ]; *)
    ]
