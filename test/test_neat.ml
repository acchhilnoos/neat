let c_iid () =
  let open Neat in
  let cn = Connection.init 0 1 0 in
  Alcotest.(check int) "in id" 0 (Connection.get_i_id cn)

(* Run it *)
let () =
  let open Alcotest in
  run "Primitives"
    [
      ( "connection",
        [
          test_case "in id" `Quick c_iid
          (* test_case "Capitalization" `Quick test_capitalize; *);
        ] );
      (* "string-concat", [ test_case "String mashing" `Quick test_str_concat  ]; *)
      (* "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ]; *)
    ]
