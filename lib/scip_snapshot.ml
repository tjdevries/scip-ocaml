open Scip_proto.Scip_types

let to_string (index : index) : string list =
  let metadata = index.metadata |> Option.value_exn in
  let root = metadata.project_root in
  let root = Base.String.chop_prefix_if_exists root ~prefix:"file://" in
  List.map
    ~f:(fun doc -> Caml.Format.sprintf "%s/%s" root doc.relative_path)
    index.documents
;;

let sort_occurrences _ =
  (* Caml.List.sort (fun a b -> Scip.ScipRange.compare a.range b.range) occs *)
  assert false
;;

let list_get l n = List.nth_exn l n |> Int32.to_int_exn

let format_occurrence (occ : occurrence) : string =
  if List.length occ.range = 4 && list_get occ.range 0 <> list_get occ.range 2
  then assert false;
  let start = List.nth_exn occ.range 1 |> Int32.to_int_exn in
  let finish =
    match List.length occ.range with
    | 3 -> list_get occ.range 2
    | 4 -> list_get occ.range 3
    | _ -> assert false
  in
  let role =
    match occ.symbol_roles |> Int32.to_int_exn with
    | 1 -> "definition"
    | _ -> "reference"
  in
  let prefix = String.make start ' ' in
  let amount = finish - start in
  let underline = String.make amount '^' in
  Caml.Format.sprintf "(*%s%s %s %s *)\n" prefix underline role occ.symbol
;;

let doc_to_string (doc : document) contents : string =
  (* let occs = List.map doc.occurrences ~f:ScipRange.of_vec in *)
  let occs = Array.of_list doc.occurrences in
  let lines = String.split_on_chars ~on:[ '\n' ] contents in
  let result = ref "" in
  (* TODO: This iterates over the occurrences way more than it needs to *)
  List.iteri
    ~f:(fun i line ->
      result := !result ^ Caml.Format.sprintf "  %s\n" line;
      (* something *)
      Array.iter
        ~f:(fun o ->
          let h = List.hd_exn o.range |> Int.of_int32_exn in
          if h = i then result := !result ^ format_occurrence o else ())
        occs)
    lines;
  !result
;;
(* List.fold_left ~init:"" ~f:(fun acc line -> acc ^ "\n" ^ line) lines *)
