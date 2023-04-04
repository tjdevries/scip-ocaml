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
  let read document root =
    let path = Fpath.(root / document.relative_path) in
    Bos.OS.File.read path |> Result.ok
  ;;

  let handle_structure = Tasty.Emit.handle_structure
  let handle_signature _ document _ = Some document

  (* Helper to make a new document *)
  let make_document relative_path = default_document ~language:"ocaml" ~relative_path ()

  let get_symbols cmt_path =
    let info = CmFile.load_cmt cmt_path in
    (* TODO: Need to merge all the globals together *)
    let* relative_path = info.cmt_sourcefile in
    let document = make_document relative_path in
    match info.cmt_annots with
    | Cmt_format.Implementation tree -> Some (Tasty.Symbols.traverse document tree)
    | Cmt_format.Implementation _ -> assert false
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
    (* TODO Can you get the arguments just from Sys.argv or something? *)
    let tool_info = Some (default_tool_info ~name ~version ~arguments:[] ()) in
    let project_root = "file://" ^ Unix.realpath (Fpath.to_string root) in
    let metadata = Some (default_metadata ~project_root ~tool_info ()) in
    (* It may be possible that we don't have to lookup EVERYTHING, but for now it's fine *)
    let index_lookup = Scip_mods.IndexSymbols.init () in
    let index_lookup =
      List.fold
        ~init:index_lookup
        ~f:(fun acc cmt ->
          let document_lookup = ScipDocument.get_symbols cmt in
          match document_lookup with
          | Some lookup -> Scip_mods.IndexSymbols.merge acc lookup
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
