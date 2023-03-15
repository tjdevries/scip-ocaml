[@@@ocaml.warning "-11-32"]

open Stdune
open Scip_ocaml.Scip_types
open Scip_ocaml.Scip_pp

let () = print_endline "Hello, World!"

type cm_file = Cmt of string | Cmti of string

let string_of_cm cm = match cm with Cmt f | Cmti f -> f
let is_directory dir = try Sys.is_directory dir with Sys_error _ -> false

let find_cm_files dir =
  let choose_file f1 f2 =
    match (f1, f2) with
    | (Cmt _ as f), _ | _, (Cmt _ as f) -> f
    | (Cmti _ as f), Cmti _ -> f
  in
  (* TODO we could get into a symlink loop here so we should we be careful *)
  let rec loop acc dir =
    let contents = Sys.readdir dir in
    Array.fold_left contents ~init:acc ~f:(fun acc fname ->
        let path = Filename.concat dir fname in
        if is_directory path then loop acc path
        else
          match String.rsplit2 ~on:'.' path with
          | Some (path_without_ext, "cmt") ->
              String.Map.set acc path_without_ext (Cmt path)
          | Some (path_without_ext, "cmti") -> (
              let current_file = String.Map.find acc path_without_ext in
              let cmi_file = Cmti path in
              match current_file with
              | None -> String.Map.set acc path_without_ext cmi_file
              | Some current_file ->
                  String.Map.set acc path_without_ext
                    (choose_file current_file cmi_file))
          | _ -> acc)
  in
  loop String.Map.empty dir |> String.Map.values

let add_document (index : index) document =
  (* let _ = Query_protocol.Jump *)
  (* TODO: Need a way to add external symbols from this document asdf *)
  { index with documents = document :: index.documents }

let print_cmt cmt =
  let open Cmt_format in
  match cmt.cmt_annots with
  | Implementation structure ->
      Load_path.init cmt.cmt_loadpath;
      let map =
        {
          Tast_mapper.default with
          env = (fun _ env -> Envaux.env_of_only_summary env);
        }
      in
      let structure = map.structure map structure in
      Format.printf "Impl: %a@." Printtyped.implementation structure
  | _ -> failwith "not a structure"

(* let print_cmi (cmi : Cmi_format.cmi_infos) = *)
(*   let open Cmt_format in *)
(*   match cmi.cmt_annots with *)
(*   | Implementation structure -> *)
(*       Load_path.init cmi.cmt_loadpath; *)
(*       let map = *)
(*         { *)
(*           Tast_mapper.default with *)
(*           env = (fun _ env -> Envaux.env_of_only_summary env); *)
(*         } *)
(*       in *)
(*       let structure = map.structure map structure in *)
(*       Format.printf "Impl: %a@." Printtyped.implementation structure *)
(*   | _ -> failwith "not a structure" *)

let index project_root =
  let tool_info =
    Some (default_tool_info ~name:"scip-ocaml" ~version:"0.0.1" ())
  in
  let metadata = default_metadata ~project_root ~tool_info () in
  let metadata = Some metadata in
  let x = default_index ~metadata () in
  let x = add_document x (default_document ()) in
  if false then Format.printf "x => %a@." pp_index x;

  let cmt_files = find_cm_files "." in
  List.iter cmt_files ~f:(fun f ->
      Format.printf "cmt file => %s@." (string_of_cm f));

  (* Compile_common.re *)
  let f = List.nth cmt_files 2 |> Option.value_exn in
  let filename = string_of_cm f in
  let _, cmt_info = Cmt_format.read filename in
  let cmt_info = Option.value_exn cmt_info in
  print_cmt cmt_info

(* let cmi_infos = Option.value_exn cmi_infos in *)
(* let _ = cmi_infos in *)
(* assert false *)
(* print_cmi cmi_info *)
(* Format.printf "filename => %s@." cmi_info *)

let () = index "test"
