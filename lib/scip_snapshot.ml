open Scip_types

let to_string (_ : index) : string = "hello world"

let sort_occurrences occs =
  List.sort (fun a b -> Scip.ScipRange.compare a.range b.range) occs
;;

let list_get l n = List.nth l n |> Int32.to_int

let format_occurrence (occ : occurrence) : string =
  let open Stdune in
  if List.length occ.range = 4 && list_get occ.range 0 != list_get occ.range 2
  then assert false;
  let start = List.nth occ.range 1 |> Option.value_exn |> Int32.to_int in
  let finish =
    match List.length occ.range with
    | 3 -> list_get occ.range 2
    | 4 -> list_get occ.range 3
    | _ -> assert false
  in
  let role =
    match occ.symbol_roles |> Int32.to_int with
    | 1 -> "definition"
    | _ -> "reference"
  in
  let prefix = String.make start ' ' in
  let amount = finish - start in
  let underline = String.make amount '^' in
  Format.sprintf "(*%s%s %s %s *)\n" prefix underline role occ.symbol
;;

let doc_to_string (doc : document) contents : string =
  let open Stdune in
  (* let occs = List.map doc.occurrences ~f:ScipRange.of_vec in *)
  let occs = Array.of_list doc.occurrences in
  let lines = String.split_on_char ~sep:'\n' contents in
  let result = ref "" in
  List.iteri lines ~f:(fun i line ->
    result := !result ^ Format.sprintf "  %s\n" line;
    (* something *)
    Array.iter occs ~f:(fun o ->
      if List.hd o.range = Int32.of_int i
      then result := !result ^ format_occurrence o
      else ())
    (* Array. *)
    (* let occ = List.hd  *)
    (* another *)
    (* another *)
    (* another *)
    (* another *));
  !result
;;
(* List.fold_left ~init:"" ~f:(fun acc line -> acc ^ "\n" ^ line) lines *)
