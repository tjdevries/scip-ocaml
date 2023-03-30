(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

open Scip_ocaml
open Scip_proto.Scip_types
open Scip_proto

(* This can let us get a parse tree for this *)
(* let somethin = Lexing.from_string "let x = 5" |> Parse.expression *)
(* module TestableSymbol : Alcotest.TESTABLE = struct *)
(*   type t = Scip_types.symbol *)
(*   let pp = Scip_pp.pp_symbol *)
(*   let equal a b = a = b *)
(* end *)

let testable_symbol = Alcotest.testable Scip_pp.pp_symbol (fun a b -> a = b)

let test_local_symbol () =
  Alcotest.(check string) "same string" "local 1" (ScipSymbol.new_local 1)
;;

let pkg manager name version = default_package ~manager ~name ~version ()

let sym scheme manager name version descriptors =
  let package = pkg manager name version in
  let package = Some package in
  Scip_types.default_symbol ~scheme ~package ~descriptors ()
;;

let test_simple_scheme () =
  let expected =
    sym "a" "b" "c" "d" [ default_descriptor ~name:"term" ~suffix:Term () ]
  in
  Alcotest.(check testable_symbol)
    "simple symbol"
    expected
    (ScipSymbol.of_string "a b c d term." |> Result.get_ok)
;;

let test_namespaces_symbol () =
  let symbol = "scip-ocaml opam merlin 14.0 lib/Something#term." in
  let expected =
    sym
      "scip-ocaml"
      "opam"
      "merlin"
      "14.0"
      [ default_descriptor ~name:"lib" ~suffix:Namespace ()
      ; default_descriptor ~name:"Something" ~suffix:Type ()
      ; default_descriptor ~name:"term" ~suffix:Term ()
      ]
  in
  Alcotest.(check testable_symbol)
    "namespaced symbol"
    expected
    (ScipSymbol.of_string symbol |> Result.get_ok)
;;

let test_roundtrip () =
  let symbols =
    [ "scip-ocaml opam merlin 14.0 lib/Something#term."
    ; "lsif-java maven package 1.0.0 java/io/File#Entry.method(+1).(param)[TypeParam]"
    ; "rust-analyzer cargo std 1.0.0 macros/println!"
    ; "a b c d `e f`."
    ]
  in
  Alcotest.(check (list string))
    "round trip"
    symbols
    (List.map
       (fun s -> ScipSymbol.of_string s |> Result.get_ok |> ScipSymbol.to_string)
       symbols)
;;

(* Run it *)
let () =
  let open Alcotest in
  run
    "Scip_symbol"
    [ ( "symbols"
      , [ test_case "local symbol" `Quick test_local_symbol
        ; test_case "parses simple" `Quick test_simple_scheme
        ; test_case "parses namespaced" `Quick test_namespaces_symbol
        ; test_case "round trip" `Quick test_roundtrip
        ] )
    ]
;;
