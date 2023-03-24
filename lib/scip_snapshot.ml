open Stdune
open Scip_types

let to_string (_ : index) : string = "hello world"

let doc_to_string (doc : document) contents : string =
  (* let occs = List.map doc.occurrences ~f:ScipRange.of_vec in *)
  let _ = doc.occurrences in
  let lines = String.split_on_char ~sep:'\n' contents in
  List.fold_left ~init:"" ~f:(fun acc line -> acc ^ "\n" ^ line) lines
;;
