(**************************************************************************)
(* Originally copied from:
   https://sourcegraph.com/github.com/ocaml/ocaml/-/blob/typing/tast_iterator.ml *)
(* Adapted for user -- tasty.ml :) *)
(**************************************************************************)

open Asttypes
open Typedtree
open Default
open Scip_proto.Scip_types
open Scip_mods

let make_documentation type_info = [ "```ocaml"; type_info; "```" ]

let handle_structure index_lookup document arg_structure =
  (* Rest of the stuff *)
  let relative_path = document.relative_path in
  let _ = Caml.Filename.remove_extension relative_path in
  let document = ref document in
  let add_occurence ?documentation (occ : occurrence) =
    let symbols =
      if Int32.(occ.symbol_roles = SymbolRoles.definition)
      then
        default_symbol_information ~symbol:occ.symbol ?documentation ()
        :: !document.symbols
      else !document.symbols
    in
    document := { !document with occurrences = occ :: !document.occurrences; symbols }
  in
  let expr sub expr_t =
    let _ =
      match expr_t.exp_desc with
      | Texp_ident (_, lid, value) ->
        IndexSymbols.lookup index_lookup value.val_loc
        |> Option.iter ~f:(function found ->
             let range = ScipRange.of_loc lid.loc in
             let symbol = found in
             add_occurence (default_occurrence ~range ~symbol ()))
      | _ -> ()
    in
    Default.iter.expr sub expr_t
  in
  let value_binding sub value =
    let pat = value.vb_pat in
    let range = ScipRange.of_loc pat.pat_loc in
    IndexSymbols.lookup index_lookup pat.pat_loc
    |> Option.iter ~f:(fun symbol ->
         let documentation =
           make_documentation @@ Fmt.str "%a" Printtyp.type_expr pat.pat_type
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.value_binding sub value
  in
  let module_binding sub module_ =
    let loc = module_.mb_name.loc in
    let range = ScipRange.of_loc loc in
    IndexSymbols.lookup index_lookup loc
    |> Option.iter ~f:(fun symbol ->
         let documentation =
           make_documentation @@ Fmt.str "%a" Printtyp.modtype module_.mb_expr.mod_type
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.module_binding sub module_
  in
  let iter = { Default.iter with expr; module_binding; value_binding } in
  iter.structure iter arg_structure;
  Some !document
;;
