(* open Typedtree *)

let rec path_to_string (path : Path.t) =
  let p =
    match path with
    | Path.Pident ident -> Ident.name ident
    | Path.Pdot (left, dotted) ->
      Format.sprintf "(dotted) %s.%s" (path_to_string left) dotted
    | Path.Papply (_, _) -> "Papply"
  in
  p
;;

let print_long_ident ident =
  Format.asprintf "%a@." Pprintast.longident ident |> print_endline
;;

let print_type_expr (type_expr : Types.type_expr) =
  Format.printf "  type: %a@." Printtyp.type_expr type_expr
;;
