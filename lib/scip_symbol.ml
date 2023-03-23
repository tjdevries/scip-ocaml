open Angstrom
open Scip_types

let new_local_symbol a = "local " ^ a

(* TODO: not actually escaping space *)
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

let ident_char = function
  | '_' | '+' | '-' | '$' -> true
  | c when letter c -> true
  | c when digit c -> true
  | _ -> false
;;

let simple_ident = take_while1 ident_char
let escaped_ident = backtick *> take_while1 (fun c -> c <> '`') <* backtick
let ident = choice ~failure_msg:"could not get ident" [ escaped_ident; simple_ident ]

(* <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit *)
(* let simple_identifier = *)
(*   take_while1 (function *)
(*     | '_' | '+' | '-' | '$' | 'a' .. '9' -> true *)
(*     | _ -> false) *)

let to_descriptor suffix pattern =
  map ~f:(fun name -> { name; suffix; disambiguator = "" }) pattern
;;

(* // <descriptor>           ::= <namespace> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta> *)
(* // <namespace>            ::= <name> '/' *)
(* // <type>                 ::= <name> '#' *)
(* // <term>                 ::= <name> '.' *)
(* // <meta>                 ::= <name> ':' *)
(* // <method>               ::= <name> '(' <method-disambiguator> ').' *)
(* // <parameter>            ::= '(' <name> ')' *)
(* // <type-parameter>       ::= '[' <name> ']' *)
let namespace_desc = to_descriptor Namespace @@ ident <* char '/'
let type_desc = to_descriptor Type @@ ident <* char '#'
let term_desc = to_descriptor Term @@ ident <* char '.'
let meta_desc = to_descriptor Meta @@ ident <* char ':'
let method_desc = to_descriptor Method @@ ident <* lparen <* ident <* rparen
let parameter_desc = to_descriptor Parameter @@ (lparen *> ident <* rparen)
let type_parameter_desc = to_descriptor Type_parameter @@ (lbrace *> ident) <* rbrace

let this_desc =
  choice
    ~failure_msg:"Failed to find descriptor"
    [ term_desc
    ; namespace_desc
    ; type_desc
    ; method_desc
    ; type_parameter_desc
    ; parameter_desc
    ; meta_desc
    ]
;;

let space = char ' '

let space_escaped =
  take_while1 (function
    | ' ' -> false
    | _ -> true)
  <* space
;;

let symbol =
  let* scheme = space_escaped in
  let* manager = space_escaped in
  let* name = space_escaped in
  let* version = space_escaped in
  let package = Option.some @@ default_package ~manager ~name ~version () in
  let* descriptors = many1 this_desc in
  Scip_types.default_symbol ~scheme ~package ~descriptors () |> Angstrom.return
;;

(* takes a string, returns a scip symbol *)
let parse_symbol (str : string) : Scip_types.symbol =
  let res = parse_string ~consume:All symbol str in
  match res with
  | Ok res -> res
  | Error err ->
    print_endline "OH NO ERROR";
    Format.printf "error: %s@." err;
    assert false
;;
