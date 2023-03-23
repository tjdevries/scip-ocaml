[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27"]

open Typedtree

let () = print_endline "\nHello, World!\n"

let handle_signature document (signature : signature) =
  let open Stdune in
  let for_item _ = print_endline "yayaya" in
  List.iter signature.sig_items ~f:for_item;
  Format.printf "interface: %d" (List.length signature.sig_items);
  document
;;

let position_to_range (loc : Location.t) =
  let start_ = loc.loc_start in
  let finish_ = loc.loc_end in
  [ Int32.of_int (start_.pos_lnum - 1)
  ; Int32.of_int (start_.pos_cnum - start_.pos_bol)
  ; Int32.of_int (finish_.pos_lnum - 1)
  ; Int32.of_int (finish_.pos_cnum - finish_.pos_bol)
  ]
;;

let rec path_to_string (path : Path.t) =
  let p =
    match path with
    | Path.Pident ident -> Ident.name ident
    | Path.Pdot (left, dotted) ->
      Format.sprintf "(dotted) %s.%s" (path_to_string left) dotted
    | Path.Papply (_, _) -> "Papply"
  in
  p
;;

let print_long_ident ident =
  Format.asprintf "%a@." Pprintast.longident ident |> print_endline
;;

let print_type_expr (type_expr : Types.type_expr) =
  Format.printf "  type: %a@." Printtyp.type_expr type_expr
;;

let get_type_of_expr (expr : expression) = print_endline "yayaya"

let handle_tree document structure =
  let expr sub t_expr =
    let exp_desc = t_expr.exp_desc in
    let _ =
      match exp_desc with
      | Texp_ident (path, loc, value) ->
        Format.printf "    ident: %s\n" @@ path_to_string path
      | Texp_constant c -> ()
      | Texp_let (_, value, expr) ->
        get_type_of_expr expr;
        print_endline "  let"
      (* { arg_label : arg_label; param : Ident.t; *)
      (* cases : value case list; partial : partial; } *)
      | Texp_function { arg_label; param; cases; partial } ->
        Format.printf "  f: %s\n" @@ Ident.name param
      | Texp_instvar (path, _, _) ->
        Format.printf "    instvar: %s\n" @@ path_to_string path
      | Texp_setinstvar (path, _, _, _) ->
        Format.printf "    set instvar: %s\n" @@ path_to_string path
      | Texp_construct (ident, desc, _) ->
        print_long_ident ident.txt;
        print_type_expr desc.cstr_res;
        print_endline "  construct"
      | Texp_apply (_, _)
      | Texp_match (_, _, _)
      | Texp_try (_, _)
      | Texp_tuple _
      | Texp_variant (_, _)
      | Texp_record _
      | Texp_field (_, _, _)
      | Texp_setfield (_, _, _, _)
      | Texp_array _
      | Texp_ifthenelse (_, _, _)
      | Texp_sequence (_, _)
      | Texp_while (_, _)
      | Texp_for (_, _, _, _, _, _)
      | Texp_send (_, _)
      | Texp_new (_, _, _)
      | Texp_override (_, _)
      | Texp_letmodule (_, _, _, _, _)
      | Texp_letexception (_, _)
      | Texp_assert _ | Texp_lazy _
      | Texp_object (_, _)
      | Texp_pack _ | Texp_letop _ | Texp_unreachable
      | Texp_extension_constructor (_, _)
      | Texp_open (_, _) -> print_endline "    something else"
    in
    Tast_iterator.default_iterator.expr sub t_expr
  in
  let iter =
    { Tast_iterator.default_iterator with
      expr
    ; value_binding =
        (fun this value ->
          let ty = value.vb_expr in
          let is_func =
            match ty.exp_desc with
            | Texp_function _ -> true
            | _ -> false
          in
          (* let x = ty.mb_id *)
          Format.printf "  value: %a (%b)@." Printpat.top_pretty value.vb_pat is_func;
          this.pat this value.vb_pat;
          this.expr this value.vb_expr)
    ; structure =
        (fun this structure ->
          print_endline "structure";
          (* let _ = *)
          (*   match structure with { str_items; str_type; str_final_env } -> () *)
          (* in *)
          Tast_iterator.default_iterator.structure this structure)
    ; structure_item =
        (fun this item ->
          print_endline "  item";
          let _ = item.str_env in
          let _ =
            match item.str_desc with
            | Tstr_value (_, value) -> ()
            | Tstr_eval (_, _)
            | Tstr_primitive _
            | Tstr_type (_, _)
            | Tstr_typext _
            | Tstr_exception _
            | Tstr_module _
            | Tstr_recmodule _
            | Tstr_modtype _
            | Tstr_open _
            | Tstr_class _
            | Tstr_class_type _
            | Tstr_include _
            | Tstr_attribute _ -> ()
          in
          Tast_iterator.default_iterator.structure_item this item)
    }
  in
  iter.structure iter structure;
  let open Scip_ocaml.Scip_types in
  let handle_value document (value : value_binding) =
    let pat = value.vb_pat in
    match pat.pat_desc with
    | Tpat_var (ident, loc) ->
      let range = position_to_range loc.loc in
      let symbol = "local " ^ Ident.name ident in
      let symbol_roles = Int32.of_int 1 in
      Format.printf "  ident: %s %s@." loc.txt (Ident.name ident);
      { document with
        occurrences =
          default_occurrence ~range ~symbol ~symbol_roles () :: document.occurrences
      }
    | Tpat_any ->
      print_endline "  got any";
      document
    | Tpat_constant _ ->
      print_endline "  got a constant";
      document
    (* | Tpat_alias (_, _, _) -> _ *)
    (* | Tpat_tuple _ -> _ *)
    (* | Tpat_construct (_, _, _, _) -> _ *)
    (* | Tpat_variant (_, _, _) -> _ *)
    (* | Tpat_record (_, _) -> _ *)
    (* | Tpat_array _ -> _ *)
    (* | Tpat_lazy _ -> _ *)
    (* | Tpat_or (_, _, _) -> _ *)
    | _ ->
      print_endline "  some other thing";
      document
  in
  let for_item document (item : structure_item) =
    print_endline "got structure_item";
    match item.str_desc with
    (* | Tstr_value (_, value) -> *)
    (*     List.fold_left value ~f:handle_value ~init:document *)
    (* | Tstr_eval (_, _) -> _ *)
    (* | Tstr_primitive _ -> _ *)
    (* | Tstr_type (_, _) -> _ *)
    (* | Tstr_typext _ -> _ *)
    (* | Tstr_exception _ -> _ *)
    (* | Tstr_module _ -> _ *)
    (* | Tstr_recmodule _ -> _ *)
    (* | Tstr_modtype _ -> _ *)
    (* | Tstr_open _ -> _ *)
    (* | Tstr_class _ -> _ *)
    (* | Tstr_class_type _ -> _ *)
    (* | Tstr_include _ -> _ *)
    (* | Tstr_attribute _ -> _ *)
    | _ -> document
  in
  (* List.iter structure.str_items ~f:for_item; *)
  (* List.fold_left structure.str_items ~init:document ~f:for_item *)
  (* print_endline "got a tree"; *)
  document
;;

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
;;
