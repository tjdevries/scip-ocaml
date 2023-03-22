[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27"]

let () = print_endline "\nHello, World!\n"

let handle_signature document (signature : Typedtree.signature) =
  let open Stdune in
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

let rec path_to_string (path : Path.t) =
  let p =
    match path with
    | Path.Pident ident -> Ident.name ident
    | Path.Pdot (left, dotted) ->
        Format.sprintf "(dotted) %s.%s" (path_to_string left) dotted
    | Path.Papply (_, _) -> "Papply"
  in
  p

let handle_tree document (structure : Typedtree.structure) =
  let iter =
    {
      Tast_iterator.default_iterator with
      value_binding =
        (fun this value ->
          print_endline "value";
          Tast_iterator.default_iterator.value_binding this value);
      structure =
        (fun this structure ->
          print_endline "structure";
          (* let _ = *)
          (*   match structure with { str_items; str_type; str_final_env } -> () *)
          (* in *)
          Tast_iterator.default_iterator.structure this structure);
      structure_item =
        (fun this item ->
          print_endline "  item";
          let _ = item.str_env in
          let _ =
            match item.str_desc with
            | Typedtree.Tstr_value (_, value) -> ()
            | Typedtree.Tstr_eval (_, _)
            | Typedtree.Tstr_primitive _
            | Typedtree.Tstr_type (_, _)
            | Typedtree.Tstr_typext _ | Typedtree.Tstr_exception _
            | Typedtree.Tstr_module _ | Typedtree.Tstr_recmodule _
            | Typedtree.Tstr_modtype _ | Typedtree.Tstr_open _
            | Typedtree.Tstr_class _ | Typedtree.Tstr_class_type _
            | Typedtree.Tstr_include _ | Typedtree.Tstr_attribute _ ->
                ()
          in
          Tast_iterator.default_iterator.structure_item this item);
      expr =
        (fun this expr ->
          let _ =
            match expr.exp_desc with
            | Texp_ident (path, loc, value) ->
                Format.printf "    ident: %s\n" @@ path_to_string path
            | Texp_constant c -> ()
            | Typedtree.Texp_let (_, _, _) -> print_endline "  let"
            (* { arg_label : arg_label; param : Ident.t; *)
            (* cases : value case list; partial : partial; } *)
            | Typedtree.Texp_function f ->
                let x = f.partial in
                let y = f.arg_label in
                print_endline "  f"
            | Typedtree.Texp_instvar (path, _, _) ->
                Format.printf "    instvar: %s\n" @@ path_to_string path
            | Typedtree.Texp_setinstvar (path, _, _, _) ->
                Format.printf "    set instvar: %s\n" @@ path_to_string path
            | Typedtree.Texp_apply (_, _)
            | Typedtree.Texp_match (_, _, _)
            | Typedtree.Texp_try (_, _)
            | Typedtree.Texp_tuple _
            | Typedtree.Texp_construct (_, _, _)
            | Typedtree.Texp_variant (_, _)
            | Typedtree.Texp_record _
            | Typedtree.Texp_field (_, _, _)
            | Typedtree.Texp_setfield (_, _, _, _)
            | Typedtree.Texp_array _
            | Typedtree.Texp_ifthenelse (_, _, _)
            | Typedtree.Texp_sequence (_, _)
            | Typedtree.Texp_while (_, _)
            | Typedtree.Texp_for (_, _, _, _, _, _)
            | Typedtree.Texp_send (_, _)
            | Typedtree.Texp_new (_, _, _)
            | Typedtree.Texp_override (_, _)
            | Typedtree.Texp_letmodule (_, _, _, _, _)
            | Typedtree.Texp_letexception (_, _)
            | Typedtree.Texp_assert _ | Typedtree.Texp_lazy _
            | Typedtree.Texp_object (_, _)
            | Typedtree.Texp_pack _ | Typedtree.Texp_letop _
            | Typedtree.Texp_unreachable
            | Typedtree.Texp_extension_constructor (_, _)
            | Typedtree.Texp_open (_, _) ->
                print_endline "    something else"
          in
          Tast_iterator.default_iterator.expr this expr);
    }
  in
  iter.structure iter structure;

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
    (* | Typedtree.Tstr_value (_, value) -> *)
    (*     List.fold_left value ~f:handle_value ~init:document *)
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
  (* List.fold_left structure.str_items ~init:document ~f:for_item *)
  (* print_endline "got a tree"; *)
  document

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
