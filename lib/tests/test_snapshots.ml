open Scip_proto.Scip_types
open Scip_ocaml.Scip_snapshot

let test_empty_doc () =
  let contents = "let _ = 5" in
  Alcotest.(check string)
    "handles empty doc"
    contents
    (doc_to_string (default_document ()) contents)
;;

let () =
  let open Alcotest in
  run
    "Scip_snapshot"
    [ "document snapshot", [ test_case "local symbol" `Quick test_empty_doc ] ]
;;
