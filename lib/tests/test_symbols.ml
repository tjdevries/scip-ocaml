(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

open Scip_ocaml
open Scip_types

(* This can let us get a parse tree for this *)
(* let somethin = Lexing.from_string "let x = 5" |> Parse.expression *)
(* module TestableSymbol : Alcotest.TESTABLE = struct *)
(*   type t = Scip_types.symbol *)
(*   let pp = Scip_pp.pp_symbol *)
(*   let equal a b = a = b *)
(* end *)

let testable_symbol = Alcotest.testable Scip_pp.pp_symbol (fun a b -> a = b)

let test_local_symbol () =
  Alcotest.(check string) "same string" "local a" (Scip_symbol.new_local_symbol "a")
;;

let test_simple_scheme () =
  let scheme = "a" in
  let package = Some (default_package ~manager:"b" ~name:"c" ~version:"d" ()) in
  let name = "term" in
  let suffix = Scip_types.Term in
  let descriptors = [ default_descriptor ~name ~suffix () ] in
  Alcotest.(check testable_symbol)
    "simple symbol"
    (Scip_types.default_symbol ~scheme ~package ~descriptors ())
    (Scip_symbol.parse_symbol "a b c d term.")
;;

(* Run it *)
let () =
  let open Alcotest in
  run
    "Scip_symbol"
    [ ( "symbols"
      , [ test_case "local symbol" `Quick test_local_symbol
        ; test_case "parses simple" `Quick test_simple_scheme
        ] )
    ]
;;
