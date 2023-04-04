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

let payload_to_str (payload : Parsetree.payload) =
  match payload with
  | Parsetree.PStr structure ->
    List.fold structure ~init:"" ~f:(fun acc str_item ->
      match str_item.pstr_desc with
      (* | Parsetree.Pstr_eval (expr, _) -> acc ^ Fmt.str "%a" Pprintast.expression expr *)
      | Parsetree.Pstr_eval (expr, _) ->
        (match expr.pexp_desc with
         | Pexp_constant (Pconst_string (str, _, _)) -> acc ^ str
         | _ -> acc)
      | _ -> acc)
  | _ -> assert false
;;

let make_documentation ?plaintext type_info =
  let result = [ "```ocaml"; type_info; "```" ] in
  match plaintext with
  | Some text -> result @ [ text ]
  | None -> result
;;

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
      | Texp_record { fields; extended_expression; _ } ->
        let _ = extended_expression in
        Array.iter
          ~f:(function
            | _, Kept _ -> ()
            | label, Overridden (lid, _) ->
              IndexSymbols.lookup index_lookup label.lbl_loc
              |> Option.iter ~f:(fun symbol ->
                   let range = ScipRange.of_loc lid.loc in
                   add_occurence @@ default_occurrence ~range ~symbol ()))
          fields
      | _ -> ()
    in
    Default.iter.expr sub expr_t
  in
  let value_binding sub value =
    let pat = value.vb_pat in
    IndexSymbols.lookup index_lookup pat.pat_loc
    |> Option.iter ~f:(fun symbol ->
         let range = ScipRange.of_loc pat.pat_loc in
         let documentation =
           make_documentation @@ Fmt.str "%a" Printtyp.type_expr pat.pat_type
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.value_binding sub value
  in
  let module_binding sub module_ =
    let loc = module_.mb_name.loc in
    IndexSymbols.lookup index_lookup loc
    |> Option.iter ~f:(fun symbol ->
         let range = ScipRange.of_loc loc in
         let documentation =
           make_documentation @@ Fmt.str "%a" Printtyp.modtype module_.mb_expr.mod_type
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.module_binding sub module_
  in
  let type_declaration this decl_t =
    IndexSymbols.lookup index_lookup decl_t.typ_name.loc
    |> Option.iter ~f:(fun symbol ->
         let range = ScipRange.of_loc decl_t.typ_name.loc in
         let documentation =
           (* make_documentation @@ Fmt.str "%a" Printtyp.type_expr pat.pat_type *)
           make_documentation @@ Fmt.str "%s" (Ident.name decl_t.typ_id)
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.type_declaration this decl_t
  in
  let label_declaration sub label =
    IndexSymbols.lookup index_lookup label.ld_loc
    |> Option.iter ~f:(fun symbol ->
         let attrs = label.ld_attributes in
         let docs =
           List.fold attrs ~init:"" ~f:(fun acc { attr_name; attr_payload; _ } ->
             match attr_name.txt with
             | "ocaml.doc" -> Fmt.str "%s%s\n" acc (payload_to_str attr_payload)
             | _ -> acc)
         in
         let range = ScipRange.of_loc label.ld_name.loc in
         let documentation =
           make_documentation ~plaintext:docs
           @@ Fmt.str
                "%s: %a"
                (Ident.name label.ld_id)
                Printtyp.type_expr
                label.ld_type.ctyp_type
         in
         add_occurence ~documentation
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.label_declaration sub label
  in
  let label_description sub (label_desc : Types.label_description) =
    (* let type_expr = label_desc.lbl_res in *)
    (* let type_str = Fmt.str "%a" Printtyp.type_expr type_expr in *)
    (* let loc = label_desc.lbl_loc in *)
    (* let loc_str = ScipLoc.of_loc loc |> ScipLoc.to_string in *)
    (* IndexSymbols.lookup index_lookup label_desc. *)
    Default.iter.label_description sub label_desc
  in
  let case sub case =
    let loc = case.c_lhs.pat_loc in
    IndexSymbols.lookup index_lookup loc
    |> Option.iter ~f:(fun symbol ->
         let range = ScipRange.of_loc loc in
         add_occurence
         @@ default_occurrence ~range ~symbol ~symbol_roles:SymbolRoles.definition ());
    Default.iter.case sub case
  in
  let iter =
    { Default.iter with
      expr
    ; case
    ; module_binding
    ; type_declaration
    ; value_binding
    ; label_declaration
    ; label_description
    }
  in
  iter.structure iter arg_structure;
  Some !document
;;
