type string_to_loc = string Map.M(ScipLoc).t

type t =
  { path : string
  ; globals : string_to_loc
  ; locals : string_to_loc
  }

let init (document : Scip_proto.Scip_types.document) globals locals =
  { path = document.relative_path; globals; locals }
;;

let lookup (_ : t) loc =
  let _ = ScipLoc.of_loc loc in
  (* match Map.find this.globals loc with *)
  (* | Some symbol -> Some symbol *)
  (* | None -> Map.find this.locals loc *)
  assert false
;;
