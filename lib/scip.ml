open Scip_proto.Scip_types
module Dir = Bos.OS.Dir

let ( let+ ) = Option.map
let ( let* ) v f = Option.bind ~f v

module CmFile = struct
  type t =
    | Cmt of Fpath.t
    | Cmti of Fpath.t

  let of_string s =
    match Fpath.of_string s with
    | Ok path when String.(Fpath.get_ext path = "cmt") -> Some (Cmt path)
    | Ok path when String.(Fpath.get_ext path = "cmti") -> Some (Cmti path)
    | _ -> None
  ;;

  let to_string = function
    | Cmt s -> Fpath.to_string s
    | Cmti s -> Fpath.to_string s
  ;;

  let load_cmt t = Cmt_format.read_cmt @@ to_string t
end

module ScipRange = struct
  type t = int32 list [@@deriving sexp]

  (* TODO: Handle different length ranges *)
  let compare a b = compare a b

  let of_vec v =
    match List.length v with
    | 3 -> v
    | 4 -> v
    | _ -> assert false
  ;;

  let of_loc (loc : Warnings.loc) : t =
    let start_ = loc.loc_start in
    let finish_ = loc.loc_end in
    [ Int32.of_int_exn (start_.pos_lnum - 1)
    ; Int32.of_int_exn (start_.pos_cnum - start_.pos_bol)
    ; Int32.of_int_exn (finish_.pos_lnum - 1)
    ; Int32.of_int_exn (finish_.pos_cnum - finish_.pos_bol)
    ]
  ;;

  let to_list this : int32 list = this
  let to_string this = Fmt.str "%s" (Sexp.to_string_hum (sexp_of_t this))
end

module StringMap = Map.M (String)

let empty = Map.empty (module String)

let find_cm_files (dir : Fpath.t) =
  let choose_file f1 f2 =
    let open CmFile in
    match f1, f2 with
    | (Cmt _ as f), _ | _, (Cmt _ as f) -> f
    | (Cmti _ as f), Cmti _ -> f
  in
  (* TODO we could get into a symlink loop here so we should we be careful *)
  let rec loop acc dir =
    let contents =
      match Dir.contents ~dotfiles:true ~rel:false dir with
      | Ok contents -> contents
      | msg -> Rresult.R.failwith_error_msg msg
    in
    let handle_file acc path =
      let path_without_ext = Fpath.(path |> rem_ext |> to_string) in
      match Fpath.get_ext path with
      | ".cmt" -> Map.set acc ~key:path_without_ext ~data:(CmFile.Cmt path)
      | ".cmti" ->
        let current_file = Map.find acc path_without_ext in
        let cmi_file = CmFile.Cmti path in
        (match current_file with
         | None -> Map.set acc ~key:path_without_ext ~data:cmi_file
         | Some current_file ->
           Map.set acc ~key:path_without_ext ~data:(choose_file current_file cmi_file))
      | _ -> acc
    in
    List.fold contents ~init:acc ~f:(fun acc path ->
      let is_directory =
        match Dir.exists path with
        | Ok is_directory -> is_directory
        | _ -> assert false
      in
      (* TODO: Probably have some other directories we could skip? Could be configured *)
      match path, is_directory with
      | path, true when String.(Fpath.basename path = ".git") -> acc
      | path, true -> loop acc path
      | path, false -> handle_file acc path)
  in
  loop empty dir |> Map.to_alist |> List.map ~f:(fun (_, v) -> v)
;;

module SymbolRoles = struct
  let definition = Int32.of_int_exn 1
end

(* NOTE: descriptors is reversed from the way that you're going to actually
   use them, since that allows you to super easily pop on and off the scope.

   We just reverse it when we make a symbol... maybe that's stupid :) *)
let make_symbol ~descriptors ~name ~suffix ?disambiguator () =
  let descriptors = default_descriptor ~name ~suffix ?disambiguator () :: descriptors in
  default_symbol
    ~scheme:"scip-ocaml"
    ~package:None
    ~descriptors:(List.rev descriptors)
    ()
;;

let make_documentation type_info = [ "```ocaml"; type_info; "```" ]

module ScipDocument = struct
  open Typedtree

  let read document root =
    let path = Fpath.(root / document.relative_path) in
    Bos.OS.File.read path |> Result.ok
  ;;

  let handle_structure index_lookup document structure =
    let open Symbol_iter in
    (* Rest of the stuff *)
    let relative_path = document.relative_path in
    let _ = Caml.Filename.remove_extension relative_path in
    let document = ref document in
    let add_occurence ?documentation (occ : occurrence) =
      let symbols =
        if Int32.(occ.symbol_roles = SymbolRoles.definition)
        then
          default_symbol_information ~symbol:occ.symbol ?documentation ()
          :: !document.symbols
        else !document.symbols
      in
      document := { !document with occurrences = occ :: !document.occurrences; symbols }
    in
    let expr sub t_expr =
      let exp_desc = t_expr.exp_desc in
      let _ =
        match exp_desc with
        | Texp_ident (_, loc, value) ->
          (* let loc = Path.head path in *)
          let looked_up = IndexSymbols.lookup index_lookup value.val_loc in
          let _ =
            match looked_up with
            | Some found ->
              let range = ScipRange.of_loc loc.loc in
              let symbol = found in
              add_occurence (default_occurrence ~range ~symbol ())
            | None -> ()
          in
          ()
        | Texp_constant _ -> ()
        | Texp_let (_, _, _) -> ()
        (* { arg_label : arg_label; param : Ident.t; *)
        (* cases : value case list; partial : partial; } *)
        (* | Texp_function { arg_label; param; cases; partial } -> Format.printf "  f: %s\n" @@ Ident.name param *)
        (* | Texp_instvar (path, _, _) -> Format.printf "    instvar: %s\n" @@ path_to_string path *)
        (* | Texp_setinstvar (path, _, _, _) -> Format.printf "    set instvar: %s\n" @@ path_to_string path *)
        (* | Texp_construct (ident, desc, _) -> *)
        (*   print_long_ident ident.txt; *)
        (*   print_type_expr desc.cstr_res; *)
        (*   print_endline "  construct" *)
        | _ -> ()
      in
      Tast_iterator.default_iterator.expr sub t_expr
    in
    (* pat: 'k . iterator -> 'k general_pattern -> unit; *)
    let pat this p =
      (* pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes *)
      let loc = p.pat_loc in
      let looked_up = IndexSymbols.lookup index_lookup loc in
      let _ =
        match looked_up with
        | Some found ->
          let range = ScipRange.of_loc loc in
          let symbol = found in
          add_occurence (default_occurrence ~range ~symbol ())
        | None -> ()
      in
      (* let _ = *)
      (*   match p.pat_desc with *)
      (*   | Tpat_any -> () *)
      (*   | Tpat_var (_, loc) -> *)
      (*     let looked_up = DocumentSymbols.lookup symbol_lookup loc.loc in *)
      (*     let _ = *)
      (*       match looked_up with *)
      (*       | Some found -> *)
      (*         let range = ScipRange.of_loc loc.loc in *)
      (*         let symbol = found in *)
      (*         add_occurence (default_occurrence ~range ~symbol ()) *)
      (*       | None -> () *)
      (*     in *)
      (*     print_endline "  var" *)
      (*   | _ -> () *)
      (* in *)
      Tast_iterator.default_iterator.pat this p
    in
    let iter =
      { Tast_iterator.default_iterator with
        expr
      ; pat
      ; value_binding =
          (fun this value ->
            let pat = value.vb_pat in
            let range = ScipRange.of_loc pat.pat_loc in
            (match IndexSymbols.lookup index_lookup pat.pat_loc with
             | Some symbol ->
               let documentation =
                 make_documentation @@ Fmt.str "%a" Printtyp.type_expr pat.pat_type
               in
               add_occurence ~documentation
               @@ default_occurrence
                    ~range
                    ~symbol
                    ~symbol_roles:SymbolRoles.definition
                    ()
             | None -> ());
            (* Normally you'd visit this pattern, but we're handling this here. *)
            (* this.pat this value.vb_pat; *)
            this.expr this value.vb_expr)
      ; module_binding =
          (fun this module_ ->
            let loc = module_.mb_name.loc in
            let range = ScipRange.of_loc loc in
            (match IndexSymbols.lookup index_lookup loc with
             | Some symbol ->
               let documentation =
                 make_documentation
                 @@ Fmt.str "%a" Printtyp.modtype module_.mb_expr.mod_type
               in
               add_occurence ~documentation
               @@ default_occurrence
                    ~range
                    ~symbol
                    ~symbol_roles:SymbolRoles.definition
                    ()
             | None -> ());
            Tast_iterator.default_iterator.module_binding this module_)
      ; structure_item =
          (fun this item ->
            let _ = item.str_env in
            let _ =
              match item.str_desc with
              | Tstr_value (_, _) -> ()
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
    Some !document
  ;;

  let handle_signature _ document _ = Some document

  (* Helper to make a new document *)
  let make_document relative_path = default_document ~language:"ocaml" ~relative_path ()

  let get_symbols cmt_path =
    let info = CmFile.load_cmt cmt_path in
    (* TODO: Need to merge all the globals together *)
    let* relative_path = info.cmt_sourcefile in
    let document = make_document relative_path in
    match info.cmt_annots with
    | Cmt_format.Implementation tree -> Some (Symbol_iter.traverse document tree)
    | Cmt_format.Interface _ -> None
    | _ -> failwith "not a cmti file"
  ;;

  let of_cmt index_lookup cmt_path =
    let info = CmFile.load_cmt cmt_path in
    let* relative_path = info.cmt_sourcefile in
    let document = make_document relative_path in
    match info.cmt_annots with
    | Cmt_format.Implementation structure ->
      handle_structure index_lookup document structure
    | Cmt_format.Interface signature -> handle_signature index_lookup document signature
    | _ -> failwith "not a cmti file"
  ;;
end

module ScipIndex = struct
  type t = index

  let name = "scip-ocaml"
  let version = "0.1"

  let index root cmt_files =
    let open Symbol_iter in
    (* TODO Can you get the arguments just from Sys.argv or something? *)
    let tool_info = Some (default_tool_info ~name ~version ~arguments:[] ()) in
    let project_root = "file://" ^ Fpath.to_string root in
    let metadata = Some (default_metadata ~project_root ~tool_info ()) in
    (* It may be possible that we don't have to lookup EVERYTHING, but for now it's fine *)
    let index_lookup = IndexSymbols.init () in
    let index_lookup =
      List.fold
        ~init:index_lookup
        ~f:(fun acc cmt ->
          let document_lookup = ScipDocument.get_symbols cmt in
          match document_lookup with
          | Some lookup -> IndexSymbols.merge acc lookup
          | None -> acc)
        cmt_files
    in
    (* TODO: Gotta think about how this works with external symbols *)
    let documents =
      List.fold_left cmt_files ~init:[] ~f:(fun acc cmt ->
        match ScipDocument.of_cmt index_lookup cmt with
        | Some doc -> doc :: acc
        | None ->
          Fmt.epr "Couldn't load %s" (CmFile.to_string cmt);
          acc)
    in
    default_index ~metadata ~documents ()
  ;;

  let serialize index outfile =
    let p_encoder = Pbrt.Encoder.create () in
    let write_index = Scip_proto.Scip_pb.encode_index index in
    write_index p_encoder;
    let bytes = Pbrt.Encoder.to_string p_encoder in
    Bos.OS.File.write outfile bytes
  ;;

  let deserialize infile =
    match Bos.OS.File.read infile with
    | Ok contents ->
      let p_decoder = Pbrt.Decoder.of_string contents in
      Some (Scip_proto.Scip_pb.decode_index p_decoder)
    | _ -> None
  ;;
end
