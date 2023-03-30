open Base
open Scip_proto.Scip_types
open Scip_ocaml

let index project_root outfile =
  Fmt.pr "@.Indexing project at %s@." (Fpath.to_string project_root);
  let files = Scip.find_cm_files project_root in
  let index = Scip.ScipIndex.index project_root files in
  let _ = Scip.ScipIndex.serialize index Fpath.(project_root / outfile) in
  ()
;;

let snapshot project_root outfile =
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
        Scip.ScipDocument.read document project_root
        |> Option.map ~f:(fun snapshot -> document.relative_path, snapshot))
    in
    Map.of_alist_exn (module String) with_docs
  in
  let _ = document_to_snapshot in
  ()
;;

(* NOTE: If i want to print these, then we derive show again *)
type params =
  { command : string [@pos 0] [@docv "command"] (** Command for scip-ocaml to run *)
  ; project_root : string [@pos 1] [@docv "project_root"] [@default "."]
      (** Project root to index *)
  ; outfile : string [@pos 2] [@docv "outfile"] [@default "index.scip"]
      (** File to save index. *)
  }
[@@deriving cmdliner]

let actually_run (params : params) =
  let project_root =
    match Fpath.of_string params.project_root with
    | Ok project_root -> project_root
    | _ -> assert false
  in
  match params.command with
  | "index" -> index project_root params.outfile
  | "snapshot" -> snapshot project_root params.outfile
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
