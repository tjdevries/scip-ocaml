(* TODO: would be nice to not have to put this at the top of the file... *)
(* gotta ask anmon how to fix I guess. *)
[@@@alert "-unstable"]

open Stdune
open Ocaml_parsing

module CmFile = struct
  type t =
    | Cmt of string
    | Cmti of string

  let of_string s =
    if Filename.check_suffix s ".cmt"
    then Cmt s
    else if Filename.check_suffix s ".cmti"
    then Cmti s
    else failwith "CmFile.of_string"
  ;;

  let to_string = function
    | Cmt s -> s
    | Cmti s -> s
  ;;

  (* todo this is so bad *)
  let load_cmt = function
    | Cmt s -> Cmt_format.read s
    | Cmti s -> Cmt_format.read s
  ;;
end

let find_cm_files dir =
  let is_directory dir =
    try Sys.is_directory dir with
    | Sys_error _ -> false
  in
  let choose_file f1 f2 =
    let open CmFile in
    match f1, f2 with
    | (Cmt _ as f), _ | _, (Cmt _ as f) -> f
    | (Cmti _ as f), Cmti _ -> f
  in
  (* TODO we could get into a symlink loop here so we should we be careful *)
  let rec loop acc dir =
    let contents = Sys.readdir dir in
    Array.fold_left contents ~init:acc ~f:(fun acc fname ->
      let path = Filename.concat dir fname in
      if is_directory path
      then loop acc path
      else (
        match String.rsplit2 ~on:'.' path with
        | Some (path_without_ext, "cmt") ->
          String.Map.set acc path_without_ext (CmFile.Cmt path)
        | Some (path_without_ext, "cmti") ->
          let current_file = String.Map.find acc path_without_ext in
          let cmi_file = CmFile.Cmti path in
          (match current_file with
           | None -> String.Map.set acc path_without_ext cmi_file
           | Some current_file ->
             String.Map.set acc path_without_ext (choose_file current_file cmi_file))
        | _ -> acc))
  in
  loop String.Map.empty dir |> String.Map.values
;;

let example _ =
  let attr _ = assert false in
  let _ = { Ast_iterator.default_iterator with signature = attr } in
  print_endline "hello world"
;;
