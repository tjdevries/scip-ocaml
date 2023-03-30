let rec path_to_string (path : Path.t) =
  let p =
    match path with
    | Path.Pident ident -> Ident.name ident
    | Path.Pdot (left, dotted) -> Fmt.str "(dotted) %s.%s" (path_to_string left) dotted
    | Path.Papply (_, _) -> "Papply"
  in
  p
;;

let print_long_ident ident = Fmt.str "%a@." Pprintast.longident ident

let print_type_expr (type_expr : Types.type_expr) =
  Fmt.str "%a" Printtyp.type_expr type_expr
;;
