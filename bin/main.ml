[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27"]

let () = print_endline "\nHello, World!\n"

(* let handle_signature document (signature : signature) = *)
(*   let for_item _ = print_endline "yayaya" in *)
(*   List.iter signature.sig_items ~f:for_item; *)
(*   Format.printf "interface: %d" (List.length signature.sig_items); *)
(*   document *)
(* ;; *)
(***)
(* let get_type_of_expr (expr : expression) = print_endline "yayaya" *)

let read_file filename =
  let ch = open_in filename in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content
;;

let () =
  let open Scip_ocaml.Scip in
  print_endline "trying to read a file";
  let project_root = "/home/tjdevries/build/simple/" in
  let f = project_root ^ "_build/default/bin/.main.eobjs/byte/dune__exe__Main.cmt" in
  let files = [ f ] in
  let index = ScipIndex.index project_root files in
  ScipIndex.serialize index "test.scip";
  (*  Now we can snapshot the document *)
  let document = List.hd index.documents in
  let contents = read_file (project_root ^ document.relative_path) in
  print_endline (Scip_ocaml.Scip_snapshot.doc_to_string document contents)
;;
