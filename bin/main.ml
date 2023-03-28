[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27"]

open Scip_proto.Scip_types

let () = print_endline "\nHello, World!\n"

(* let handle_signature document (signature : signature) = *)
(*   let for_item _ = print_endline "yayaya" in *)
(*   List.iter signature.sig_items ~f:for_item; *)
(*   Format.printf "interface: %d" (List.length signature.sig_items); *)
(*   document *)
(* ;; *)
(***)
(* let get_type_of_expr (expr : expression) = print_endline "yayaya" *)

let () =
  let open Scip_ocaml.Scip in
  print_endline "trying to read a file";
  let project_root = "/home/tjdevries/sourcegraph/scip_ocaml/" in
  (* let project_root = "/home/tjdevries/build/simple/" in *)
  let files = find_cm_files project_root in
  List.iter (fun f -> print_endline f) files;
  let index = ScipIndex.index project_root files in
  ScipIndex.serialize index (project_root ^ "test.scip");
  (*  Now we can snapshot the document *)
  List.iter
    (fun d ->
      Format.printf "==== Document: %s =====@." d.relative_path;
      let contents = ScipDocument.read d project_root in
      match contents with
      | Some contents ->
        print_endline (Scip_ocaml.Scip_snapshot.doc_to_string d contents)
      | None -> ())
    index.documents
;;
