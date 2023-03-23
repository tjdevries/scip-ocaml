(* open Typedtree *)
open Angstrom

let new_local_symbol a = "local " ^ a

(* scheme is any character, except space. Double space escapes *)
let scheme = take_while1 (function ' ' -> false | _ -> true)

(* // <package>              ::= <manager> ' ' <package-name> ' ' <version> *)
(* // <manager>              ::= same as above, use the placeholder '.' to indicate an empty value *)
(* // <package-name>         ::= same as above *)
(* // <version>              ::= same as above *)
let package = assert false

(* <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit *)
let simple_identifier =
  take_while (function
    | '_' | '+' | '-' | '$' | 'a' .. '9' -> true
    | _ -> false)

let symbol =
  (* call scheme *)
  (* let parsed_scheme >>= fun id -> others in *)
  let* parsed_scheme = scheme in
  (* call package *)
  let* parsed_package = package in
  (* call descriptors *)
  Scip_types.default_symbol ~scheme:parsed_scheme ~package:parsed_package ()
  |> Angstrom.return

(* takes a string, returns a scip symbol *)
let parse_symbol (str : string) : Scip_types.symbol =
  parse_string ~consume:All symbol str |> Result.get_ok
