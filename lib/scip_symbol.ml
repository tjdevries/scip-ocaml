open Angstrom
open Scip_types

module ScipDescriptor = struct
  type t = Scip_types.descriptor

  let to_string (desc : t) =
    let name =
      if String.contains desc.name ' ' then "`" ^ desc.name ^ "`" else desc.name
    in
    match desc.disambiguator with
    | "" ->
      Format.sprintf
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
      Format.sprintf
        (match desc.suffix with
         | Method -> "%s(%s)."
         | _ -> assert false)
        name
        disambiguator
  ;;
end

module ScipSymbol = struct
  type t = Scip_types.symbol

  (* Convert a symbol a string *)
  let to_string (sym : t) =
    let str = sym.scheme in
    let str = str ^ " " in
    let str =
      str
      ^
      match sym.package with
      | Some pkg -> pkg.manager ^ " " ^ pkg.name ^ " " ^ pkg.version ^ " "
      | None -> ""
    in
    List.fold_left
      (fun acc desc -> acc ^ ScipDescriptor.to_string desc)
      str
      sym.descriptors
  ;;

  (* Create a new local *)
  let new_local num = "local " ^ Int.to_string num

  (* Parsing some good stuff *)
  let backtick = char '`'
  let lbrace = char '['
  let rbrace = char ']'
  let lparen = char '('
  let rparen = char ')'

  let letter = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  let digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  (* <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit *)
  let ident_char = function
    | '_' | '+' | '-' | '$' -> true
    | c when letter c -> true
    | c when digit c -> true
    | _ -> false
  ;;

  let simple_ident = take_while1 ident_char
  let escaped_ident = backtick *> take_while1 (fun c -> c <> '`') <* backtick
  let ident = choice ~failure_msg:"could not get ident" [ escaped_ident; simple_ident ]

  let to_descriptor suffix pattern =
    map ~f:(fun name -> { name; suffix; disambiguator = "" }) pattern
  ;;

  (*  <namespace> ::= <name> '/' *)
  let namespace_desc = to_descriptor Namespace @@ ident <* char '/'

  (*  <type> ::= <name> '#' *)
  let type_desc = to_descriptor Type @@ ident <* char '#'

  (*  <term> ::= <name> '.' *)
  let term_desc = to_descriptor Term @@ ident <* char '.'

  (*  <meta> ::= <name> ':' *)
  let meta_desc = to_descriptor Meta @@ ident <* char ':'

  (* <macro> ::= <name> '!' *)
  let macro_desc = to_descriptor Macro @@ ident <* char '!'

  (*  <method> ::= <name> '(' <method-disambiguator> ').' *)
  let method_desc =
    let suffix = Method in
    let* name = ident <* lparen in
    let* disambiguator = take_while ident_char in
    rparen <* char '.' >>= fun _ -> return { name; suffix; disambiguator }
  ;;

  (*  <parameter> ::= '(' <name> ')' *)
  let parameter_desc = to_descriptor Parameter @@ (lparen *> ident <* rparen)

  (*  <type-parameter> ::= '[' <name> ']' *)
  let type_parameter_desc = to_descriptor Type_parameter @@ (lbrace *> ident) <* rbrace

  (*  <descriptor> ::= <namespace> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta> *)
  let this_desc =
    choice
      ~failure_msg:"Failed to find descriptor"
      [ namespace_desc
      ; type_desc
      ; term_desc
      ; method_desc
      ; type_parameter_desc
      ; parameter_desc
      ; meta_desc
      ; macro_desc
      ]
  ;;

  let space = char ' '

  (* TODO: not actually escaping space *)
  let space_escaped consume =
    let x =
      take_while1 (function
        | ' ' -> false
        | _ -> true)
    in
    if consume then x <* space else x
  ;;

  let symbol =
    let* scheme = space_escaped true in
    let* manager = space_escaped true in
    let* name = space_escaped true in
    let* version = space_escaped true in
    let package = Option.some @@ default_package ~manager ~name ~version () in
    let* descriptors = many1 this_desc in
    Scip_types.default_symbol ~scheme ~package ~descriptors () |> Angstrom.return
  ;;

  (* takes a string, returns a scip symbol *)
  let of_string (str : string) = parse_string ~consume:All symbol str
end
