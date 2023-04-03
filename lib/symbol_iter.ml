open Scip_proto.Scip_types
open Typedtree

type string_to_loc = string Map.M(ScipLoc).t

module SymbolTracker = struct
  (* Is it good or bad to do this kind of thing...
     I'm not super in love with all mutable fields.
     Doesn't feel very "ocaml" *)
  type t =
    { mutable globals : string_to_loc
    ; mutable locals : string_to_loc
    ; mutable local_idx : int
    }

  let init () =
    { globals = Map.empty (module ScipLoc)
    ; locals = Map.empty (module ScipLoc)
    ; local_idx = 0
    }
  ;;

  let get_globals this = this.globals
  let get_locals this = this.locals

  let add_global this loc symbol =
    let loc = ScipLoc.of_loc loc in
    (* TODO: Probably should not let duplicateds happen *)
    match Map.add this.globals ~key:loc ~data:symbol with
    | `Duplicate -> ()
    | `Ok globals -> this.globals <- globals
  ;;

  let add_local this loc =
    let loc = ScipLoc.of_loc loc in
    let idx = this.local_idx in
    this.local_idx <- idx + 1;
    this.locals <- Map.add_exn this.locals ~key:loc ~data:(ScipSymbol.new_local idx)
  ;;
end

module IterState = struct
  type t =
    { with_descriptor : descriptor -> (unit -> unit) -> unit
    ; get_descriptors : unit -> descriptor list
    }

  let init descriptors =
    (* Manage descriptors *)
    let with_descriptor descriptor f =
      descriptors := descriptor :: !descriptors;
      let result = f () in
      descriptors := List.tl_exn !descriptors;
      result
    in
    let get_descriptors () = !descriptors in
    { with_descriptor; get_descriptors }
  ;;
end

(* document -> string ScipLocMap.t *)
(* Map.find string -> string ScipLocaMap.t *)
module DocumentSymbols = struct
  type t =
    { path : string
    ; globals : string_to_loc
    ; locals : string_to_loc
    }

  let init document globals locals = { path = document.relative_path; globals; locals }

  let lookup this loc =
    let loc = ScipLoc.of_loc loc in
    match Map.find this.globals loc with
    | Some symbol -> Some symbol
    | None -> Map.find this.locals loc
  ;;
end

module IndexSymbols = struct
  (* TODO: locals should be multipler smaller maps, I think the lookup
           would probably be much better... but oh well*)
  type t =
    { globals : string_to_loc
    ; locals : string_to_loc
    }

  let init () =
    { globals = Map.empty (module ScipLoc); locals = Map.empty (module ScipLoc) }
  ;;

  let merge this (doc : DocumentSymbols.t) =
    let globals =
      Map.fold doc.globals ~init:this.globals ~f:(fun ~key ~data ->
        Map.add_exn ~key ~data)
    in
    let locals =
      Map.fold doc.locals ~init:this.locals ~f:(fun ~key ~data -> Map.add_exn ~key ~data)
    in
    { globals; locals }
  ;;

  let lookup this loc =
    let loc = ScipLoc.of_loc loc in
    match Map.find this.globals loc with
    | Some symbol -> Some symbol
    | None -> Map.find this.locals loc
  ;;
end

let default_package = Some (default_package ~manager:"opam" ~name:"." ~version:"." ())

(* NOTE: descriptors is reversed from the way that you're going to actually
   use them, since that allows you to super easily pop on and off the scope.

   We just reverse it when we make a symbol... maybe that's stupid :) *)
let make_symbol ~descriptors ~name ~suffix ?disambiguator () =
  let descriptors = default_descriptor ~name ~suffix ?disambiguator () :: descriptors in
  ScipSymbol.to_string
  @@ default_symbol
       ~scheme:"scip-ocaml"
       ~package:default_package
       ~descriptors:(List.rev descriptors)
       ()
;;

module PatDesc = struct
  let to_string = function
    | Tpat_any -> "any"
    | Tpat_var _ -> "var"
    | Tpat_alias _ -> "alias"
    | Tpat_constant _ -> "constant"
    | Tpat_tuple _ -> "tuple"
    | Tpat_construct _ -> "construct"
    | Tpat_variant _ -> "variant"
    | Tpat_record _ -> "record"
    | Tpat_array _ -> "array"
    | Tpat_or _ -> "or"
    | Tpat_lazy _ -> "lazy"
  ;;

  let name pattern =
    match pattern.pat_desc with
    | Tpat_var (ident, _) -> Ident.name ident
    | _ -> assert false
  ;;
end

let iter_texp_function (value : value_binding) =
  let pattern = value.vb_pat in
  let expr = value.vb_expr in
  let _ =
    match pattern.pat_desc with
    | Tpat_var (ident, _) -> Ident.name ident
    | _ -> assert false
  in
  match expr.exp_desc with
  | Texp_function { arg_label; param; cases; partial } ->
    let _ = arg_label in
    let _ = param in
    let _ = cases in
    let _ = partial in
    ()
  | _ -> assert false
;;

let is_func_expression (value : value_binding) =
  match value.vb_expr.exp_desc with
  | Texp_function _ -> true
  | _ -> false
;;

let find_symbols structure state tracker =
  let default_iterator = Tast_iterator.default_iterator in
  (* Used to generate symbols not that are local.
      Called by the iterator below *)
  let local_value_bind _ value =
    let expr = value.vb_expr in
    match expr.exp_desc with
    | Texp_function _ -> SymbolTracker.add_local tracker value.vb_pat.pat_loc
    | Texp_constant _ -> SymbolTracker.add_local tracker value.vb_pat.pat_loc
    | _ -> ()
  in
  let local_iter = { default_iterator with value_binding = local_value_bind } in
  (* Used to generate top level symbol definitions *)
  let value_binding _ value =
    let pattern = value.vb_pat in
    let expr = value.vb_expr in
    let descriptors = IterState.(state.get_descriptors ()) in
    let name =
      match pattern.pat_desc with
      | Tpat_var (ident, _) -> Ident.name ident
      | _ -> "unknown"
    in
    let symbol =
      match expr.exp_desc with
      | Texp_function { arg_label; param; cases; partial } ->
        let _ = arg_label in
        let _ = param in
        let _ = cases in
        List.iter cases ~f:(fun { c_lhs; c_guard; c_rhs } ->
          let _ = c_lhs in
          let _ =
            match c_lhs.pat_desc with
            | Tpat_var (ident, _) -> Ident.name ident
            | _ -> "unknown"
          in
          let _ = c_guard in
          let _ = c_rhs in
          ());
        let _ = partial in
        Some (make_symbol ~descriptors ~name ~suffix:Method ())
      | Texp_constant _ -> Some (make_symbol ~descriptors ~name ~suffix:Term ())
      | Texp_ident (path, _, _) ->
        let _ = path in
        None
      | _ ->
        if false
        then
          Caml.Format.printf
            "  -> unknown expr: %s %s@."
            name
            (Ttype_utils.print_type_expr expr.exp_type);
        None
    in
    (* TODO: I'm not a huge fan of this... how to write better? *)
    let _ =
      match symbol with
      | Some symbol -> SymbolTracker.add_global tracker pattern.pat_loc symbol
      | None -> ()
    in
    (* Iterate through everything else *)
    default_iterator.value_binding local_iter value
  in
  let module_binding this module_ =
    let name = module_.mb_name.txt |> Option.value_exn in
    let descriptors = IterState.(state.get_descriptors ()) in
    let symbol = make_symbol ~descriptors ~name ~suffix:Type () in
    SymbolTracker.add_global tracker module_.mb_name.loc symbol;
    (* TODO: Determine if this should be type or namespace... *)
    let module_descriptor = default_descriptor ~name ~suffix:Type () in
    state.with_descriptor module_descriptor
    @@ fun () -> default_iterator.module_binding this module_
  in
  let case : 'k. Tast_iterator.iterator -> 'k case -> unit =
   fun this c ->
    (* pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes; *)
    let _ = c.c_lhs in
    let desc = c.c_lhs.pat_desc in
    let _ = desc in
    let _ = c.c_guard in
    let _ = c.c_rhs in
    default_iterator.case this c
  in
  let type_declaration this type_declaration =
    let name = type_declaration.typ_name.txt in
    let descriptors = IterState.(state.get_descriptors ()) in
    let symbol = make_symbol ~descriptors ~name ~suffix:Type () in
    SymbolTracker.add_global tracker type_declaration.typ_name.loc symbol;
    (* TODO: Should not have to copy making the descriptor twice *)
    let type_descriptor = default_descriptor ~name ~suffix:Namespace () in
    IterState.(
      state.with_descriptor type_descriptor
      @@ fun () ->
      let _ =
        match type_declaration.typ_kind with
        | Ttype_record labels ->
          List.iter labels ~f:(fun label ->
            let descriptors = IterState.(state.get_descriptors ()) in
            let symbol =
              make_symbol ~descriptors ~name:(Ident.name label.ld_id) ~suffix:Term ()
            in
            SymbolTracker.add_global tracker label.ld_name.loc symbol)
        | _ -> ()
      in
      default_iterator.type_declaration this type_declaration)
  in
  let iter =
    { default_iterator with value_binding; module_binding; case; type_declaration }
  in
  iter.structure iter structure
;;

let traverse document structure =
  Fmt.pr "  Traversing Document: %s@." document.relative_path;
  let relative_path = document.relative_path in
  let relative_path = Caml.Filename.remove_extension relative_path in
  let descriptors =
    ref [ default_descriptor ~name:relative_path ~suffix:Namespace () ]
  in
  let state = IterState.init descriptors in
  let tracker = SymbolTracker.init () in
  find_symbols structure state tracker;
  DocumentSymbols.init
    document
    (SymbolTracker.get_globals tracker)
    (SymbolTracker.get_locals tracker)
;;

(* TODO: Create local symbol iterator... just adds to local iterator *)
