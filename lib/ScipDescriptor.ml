open Scip_proto.Scip_types

type t = descriptor

let to_string (desc : t) =
  let name =
    if String.contains desc.name ' ' then "`" ^ desc.name ^ "`" else desc.name
  in
  match desc.disambiguator with
  | "" ->
    Fmt.str
      (match desc.suffix with
       | Namespace | Package -> "%s/"
       | Type -> "%s#"
       | Term -> "%s."
       | Method -> "%s()."
       | Type_parameter -> "[%s]"
       | Parameter -> "(%s)"
       | Macro -> "%s!"
       | Meta -> "%s:"
       | Local -> "local %s"
       | Unspecified_suffix -> "%s")
      name
  | disambiguator ->
    (* As of right now, I think only methods can do stuff with disambiguator *)
    Fmt.str
      (match desc.suffix with
       | Method -> "%s(%s)."
       | _ -> assert false)
      name
      disambiguator
;;
