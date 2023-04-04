type string_to_loc = string Map.M(ScipLoc).t

(* TODO: locals should be multipler smaller maps, I think the lookup
           would probably be much better... but oh well*)
type t =
  { globals : string_to_loc
  ; locals : string_to_loc
  }

let init () =
  { globals = Map.empty (module ScipLoc); locals = Map.empty (module ScipLoc) }
;;

let merge this (doc : DocumentSymbols.t) =
  let globals =
    Map.fold doc.globals ~init:this.globals ~f:(fun ~key ~data -> Map.add_exn ~key ~data)
  in
  let locals =
    Map.fold doc.locals ~init:this.locals ~f:(fun ~key ~data -> Map.add_exn ~key ~data)
  in
  { globals; locals }
;;

let lookup this loc =
  let loc = ScipLoc.of_loc loc in
  match Map.find this.globals loc with
  | Some symbol -> Some symbol
  | None -> Map.find this.locals loc
;;
