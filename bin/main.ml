[@@@alert "-unstable"]
[@@@ocaml.warning "-26-27-32"]

open Base
open Scip_proto.Scip_types
open Scip_ocaml

let index project_root =
  Fmt.pr "Indexing project at %s@." project_root;
  let files = Scip.find_cm_files project_root in
  List.iter ~f:(fun f -> Fmt.pr " Found: %s@." f) files;
  let index = Scip.ScipIndex.index project_root files in
  Scip.ScipIndex.serialize index (project_root ^ "test.scip")
;;

let snapshot project_root =
  let document_to_snapshot =
    let path = project_root ^ "test.scip" in
    let index =
      match Scip.ScipIndex.deserialize path with
      | Some index -> index
      | None ->
        Fmt.epr "Failed to read snapshot: %s@." path;
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

(*  Now we can snapshot the documents *)

type params =
  { command : string [@pos 0] [@docv "command"] (** Command for scip-ocaml to run *)
  ; project_root : string [@pos 1] [@docv "project_root"] [@default "."]
      (** Project root to index *)
  }
[@@deriving cmdliner, show]

let actually_run (params : params) =
  match params.command with
  | "index" -> index params.project_root
  | "snapshot" -> snapshot params.project_root
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
