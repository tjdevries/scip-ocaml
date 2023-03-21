[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27"]

open Stdune

let () = print_endline "\nHello, World!\n"

let handle_signature document (signature : Typedtree.signature) =
  let for_item _ = print_endline "yayaya" in
  List.iter signature.sig_items ~f:for_item;
  Format.printf "interface: %d" (List.length signature.sig_items);
  document

let position_to_range (loc : Location.t) =
  let start_ = loc.loc_start in
  let finish_ = loc.loc_end in
  [
    Int32.of_int (start_.pos_lnum - 1);
    Int32.of_int (start_.pos_cnum - start_.pos_bol);
    Int32.of_int (finish_.pos_lnum - 1);
    Int32.of_int (finish_.pos_cnum - finish_.pos_bol);
  ]

let handle_tree document (structure : Typedtree.structure) =
  let open Scip_ocaml.Scip_types in
  let handle_value document (value : Typedtree.value_binding) =
    let pat = value.vb_pat in
    match pat.pat_desc with
    | Typedtree.Tpat_var (ident, loc) ->
        let range = position_to_range loc.loc in
        let symbol = "local " ^ Ident.name ident in
        let symbol_roles = Int32.of_int 1 in
        Format.printf "  ident: %s %s@." loc.txt (Ident.name ident);
        {
          document with
          occurrences =
            default_occurrence ~range ~symbol ~symbol_roles ()
            :: document.occurrences;
        }
    | Typedtree.Tpat_any ->
        print_endline "  got any";
        document
    | Typedtree.Tpat_constant _ ->
        print_endline "  got a constant";
        document
    (* | Typedtree.Tpat_alias (_, _, _) -> _ *)
    (* | Typedtree.Tpat_tuple _ -> _ *)
    (* | Typedtree.Tpat_construct (_, _, _, _) -> _ *)
    (* | Typedtree.Tpat_variant (_, _, _) -> _ *)
    (* | Typedtree.Tpat_record (_, _) -> _ *)
    (* | Typedtree.Tpat_array _ -> _ *)
    (* | Typedtree.Tpat_lazy _ -> _ *)
    (* | Typedtree.Tpat_or (_, _, _) -> _ *)
    | _ ->
        print_endline "  some other thing";
        document
  in
  let for_item document (item : Typedtree.structure_item) =
    print_endline "got structure_item";
    match item.str_desc with
    | Typedtree.Tstr_value (_, value) ->
        List.fold_left value ~f:handle_value ~init:document
    (* | Typedtree.Tstr_eval (_, _) -> _ *)
    (* | Typedtree.Tstr_primitive _ -> _ *)
    (* | Typedtree.Tstr_type (_, _) -> _ *)
    (* | Typedtree.Tstr_typext _ -> _ *)
    (* | Typedtree.Tstr_exception _ -> _ *)
    (* | Typedtree.Tstr_module _ -> _ *)
    (* | Typedtree.Tstr_recmodule _ -> _ *)
    (* | Typedtree.Tstr_modtype _ -> _ *)
    (* | Typedtree.Tstr_open _ -> _ *)
    (* | Typedtree.Tstr_class _ -> _ *)
    (* | Typedtree.Tstr_class_type _ -> _ *)
    (* | Typedtree.Tstr_include _ -> _ *)
    (* | Typedtree.Tstr_attribute _ -> _ *)
    | _ -> document
  in

  (* List.iter structure.str_items ~f:for_item; *)
  List.fold_left structure.str_items ~init:document ~f:for_item

(* Consider exploring this *)
(* Cmi_format.read_cmi *)

let () =
  let open Scip_ocaml.Scip_types in
  print_endline "trying to read a file";
  let f =
    "/home/tjdevries/build/simple/_build/default/bin/.main.eobjs/byte/dune__exe__Main.cmt"
  in

  let project_root = "file:///home/tjdevries/build/simple/" in
  let tool_info = Some (default_tool_info ()) in
  let metadata = default_metadata ~project_root ~tool_info () in
  let metadata = Some metadata in
  let index = default_index ~metadata () in

  let info = Cmt_format.read_cmt f in

  let relative_path = info.cmt_sourcefile in
  let document = default_document ~language:"ocaml" ?relative_path () in

  let document =
    match info.cmt_annots with
    | Cmt_format.Implementation tree -> handle_tree document tree
    | Cmt_format.Interface signature -> handle_signature document signature
    | _ -> failwith "not a cmti file"
  in

  let index = { index with documents = document :: index.documents } in
  let p_encoder = Pbrt.Encoder.create () in

  let write_index = Scip_ocaml.Scip_pb.encode_index index in
  let output = write_index p_encoder in

  let file = open_out "test.scip" in
  let bytes = Pbrt.Encoder.to_bytes p_encoder in
  output_bytes file bytes;
  close_out file;

  print_endline "read the file"
