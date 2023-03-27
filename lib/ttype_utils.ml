(* open Typedtree *)

let rec path_to_string (path : Path.t) =
  let p =
    match path with
    | Path.Pident ident -> Ident.name ident
    | Path.Pdot (left, dotted) ->
      Caml.Format.sprintf "(dotted) %s.%s" (path_to_string left) dotted
    | Path.Papply (_, _) -> "Papply"
  in
  p
;;

let print_long_ident ident =
  Caml.Format.asprintf "%a@." Pprintast.longident ident |> Caml.print_endline
;;

let print_type_expr (type_expr : Types.type_expr) =
  Caml.Format.printf "  type: %a@." Printtyp.type_expr type_expr
;;
