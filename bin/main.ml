open Base
open Scip_proto.Scip_types
open Scip_ocaml
module Dir = Bos.OS.Dir

let ( let* ) v f = Option.bind ~f v

let index project_root outfile =
  Fmt.pr "Indexing project at %s@." (Fpath.to_string project_root);
  let files = Scip.find_cm_files project_root in
  let index = Scip.ScipIndex.index project_root files in
  let _ = Scip.ScipIndex.serialize index Fpath.(project_root / outfile) in
  ()
;;

let snapshot project_root outfile snapshot_dir mode =
  Fmt.pr "  Snapshotting project at: %s@." (Fpath.to_string project_root);
  let _ = index project_root outfile in
  let document_to_snapshot =
    let path = Fpath.(project_root / outfile) in
    let index =
      match Scip.ScipIndex.deserialize path with
      | Some index -> index
      | None ->
        Fmt.epr "Failed to read snapshot: %s@." (Fpath.to_string path);
        assert false
    in
    let with_docs =
      List.filter_map index.documents ~f:(fun document ->
        let* snapshot = Scip.ScipDocument.read document project_root in
        let snapshot = Scip_snapshot.doc_to_string document snapshot in
        Some (document.relative_path, snapshot))
    in
    Map.of_alist_exn (module String) with_docs
  in
  match mode with
  | `Promote ->
    let _ = Bos.OS.Dir.create snapshot_dir in
    Map.iteri document_to_snapshot ~f:(fun ~key ~data ->
      let path = Fpath.(snapshot_dir / key) in
      let _ = Bos.OS.File.write path data in
      ());
    Map.empty (module String)
  | `Diff ->
    Map.filter_mapi document_to_snapshot ~f:(fun ~key ~data ->
      let path = Fpath.(snapshot_dir / key) in
      let existing =
        match Bos.OS.File.read path with
        | Ok existing -> existing
        | _ -> ""
      in
      (* TODO: Could do the diff probably? *)
      if String.(existing <> data) then Some key else None)
  | `Write ->
    Map.iteri document_to_snapshot ~f:(fun ~key ~data -> Fmt.pr "  %s -> %s@." key data);
    Map.empty (module String)
;;

let snapshot_dir root mode =
  let input = Fpath.(root / "input") in
  let output = Fpath.(root / "output") in
  let contents =
    match input |> Dir.contents ~rel:false with
    | Ok contents -> contents
    | _ -> assert false
  in
  let do_snapshot acc project =
    snapshot project "index.scip" output mode
    |> Map.fold ~init:acc ~f:(fun ~key ~data -> Map.add_exn ~key ~data)
  in
  let init = Map.empty (module String) in
  let diffed = List.fold contents ~init ~f:do_snapshot in
  if Map.length diffed > 0
  then (
    Fmt.pr "Snapshot diffed files: %d@." (Map.length diffed);
    Map.iteri diffed ~f:(fun ~key ~data -> Fmt.pr "  %s -> %s@." key data);
    Caml.exit 1)
;;

(* NOTE: If i want to print these, then we derive show again *)
type params =
  { command : string [@pos 0] [@docv "command"] (** Command for scip-ocaml to run *)
  ; root : string [@pos 1] [@docv "project_root"] [@default "."]
      (** Project root to index *)
  ; outfile : string [@pos 2] [@docv "outfile"] [@default "index.scip"]
      (** File to save index. *)
  ; snapshot_dir : string [@docv "snapshot_dir"] [@default "scip-snapshot"]
      (** folder to save snapshots to *)
  ; mode : string [@docv "mode"] [@default "diff"]
      (** Should promote snapshots to newest items *)
  }
[@@deriving cmdliner]

let actually_run (params : params) =
  let root =
    match Fpath.of_string params.root with
    | Ok project_root -> project_root
    | _ -> assert false
  in
  let mode =
    match params.mode with
    | "promote" -> `Promote
    | "diff" -> `Diff
    | "write" -> `Write
    | _ -> assert false
  in
  match params.command with
  | "index" -> index root params.outfile
  | "snapshot" ->
    let snapshot_dir = Fpath.(root / params.snapshot_dir) in
    let _ = snapshot root params.outfile snapshot_dir mode in
    ()
  | "snapshot-dir" ->
    let _ = snapshot_dir root mode in
    ()
  | _ -> Fmt.epr "Unknown command: %s@." params.command
;;

let main () =
  let f p = actually_run p in
  let info = Cmdliner.Cmd.info Caml.Sys.argv.(0) in
  let term = Cmdliner.Term.(const f $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  Caml.exit (Cmdliner.Cmd.eval cmd)
;;

let () = main ()
