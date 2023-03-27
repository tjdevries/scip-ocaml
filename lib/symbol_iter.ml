open Scip_loc
open Scip_types
open Typedtree
module ScipSymbol = Scip_symbol.ScipSymbol

module SymbolTracker = struct
  (* Is it good or bad to do this kind of thing...
     I'm not super in love with all mutable fields.
     Doesn't feel very "ocaml" *)
  type t =
    { mutable globals : string ScipLocMap.t
    ; mutable locals : string ScipLocMap.t
    ; mutable local_idx : int
    }

  let init () = { globals = ScipLocMap.empty; locals = ScipLocMap.empty; local_idx = 0 }
  let get_globals this = this.globals
  let get_locals this = this.locals

  let add_global this loc symbol =
    let loc = ScipLoc.of_loc loc in
    this.globals <- ScipLocMap.add loc symbol this.globals
  ;;

  let add_local this loc =
    let loc = ScipLoc.of_loc loc in
    let idx = this.local_idx in
    this.local_idx <- idx + 1;
    this.locals <- ScipLocMap.add loc (ScipSymbol.new_local idx) this.locals
  ;;
end

module IterState = struct
  type t =
    { with_descriptor : Scip_types.descriptor -> (unit -> unit) -> unit
    ; get_descriptors : unit -> Scip_types.descriptor list
    }

  let init descriptors =
    (* Manage descriptors *)
    let with_descriptor descriptor f =
      descriptors := descriptor :: !descriptors;
      let result = f () in
      descriptors := List.tl !descriptors;
      result
    in
    let get_descriptors () = !descriptors in
    { with_descriptor; get_descriptors }
  ;;
end

module SymbolLookup = struct
  type t =
    { globals : string ScipLocMap.t
    ; locals : string ScipLocMap.t
    }

  let init globals locals =
    Format.printf "globals: %d@." (ScipLocMap.cardinal globals);
    ScipLocMap.iter
      (fun k v -> Format.printf "  loc: %s -> %s@." (ScipLoc.hash k) v)
      globals;
    Format.printf "locals: %d@." (ScipLocMap.cardinal locals);
    ScipLocMap.iter
      (fun k v -> Format.printf "  loc: %s -> %s@." (ScipLoc.hash k) v)
      locals;
    { globals; locals }
  ;;

  let lookup this loc =
    let loc = ScipLoc.of_loc loc in
    match ScipLocMap.find_opt loc this.globals with
    | Some symbol -> Some symbol
    | None -> ScipLocMap.find_opt loc this.locals
  ;;
end

(* NOTE: descriptors is reversed from the way that you're going to actually
   use them, since that allows you to super easily pop on and off the scope.

   We just reverse it when we make a symbol... maybe that's stupid :) *)
let make_symbol ~descriptors ~name ~suffix ?disambiguator () =
  let open Scip_types in
  let descriptors = default_descriptor ~name ~suffix ?disambiguator () :: descriptors in
  ScipSymbol.to_string
  @@ default_symbol
       ~scheme:"scip-ocaml"
       ~package:None
       ~descriptors:(List.rev descriptors)
       ()
;;

let find_symbols structure state tracker =
  let default_iterator = Tast_iterator.default_iterator in
  (* Used to generate symbols not that are local.
      Called by the iterator below *)
  let local_value_bind this value =
    let expr = value.vb_expr in
    match expr.exp_desc with
    | Texp_function _ -> SymbolTracker.add_local tracker value.vb_pat.pat_loc
    | Texp_constant _ -> SymbolTracker.add_local tracker value.vb_pat.pat_loc
    | _ -> ()
  in
  let local_iter = { default_iterator with value_binding = local_value_bind } in
  (* Used to generate top level symbol definitions *)
  let value_binding this value =
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
      | Texp_function _ -> Some (make_symbol ~descriptors ~name ~suffix:Method ())
      | Texp_constant _ -> Some (make_symbol ~descriptors ~name ~suffix:Term ())
      | _ ->
        Format.printf "  -> unknown expr: %s@." name;
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
    let name = module_.mb_name.txt |> Option.get in
    let descriptors = IterState.(state.get_descriptors ()) in
    let symbol = make_symbol ~descriptors ~name ~suffix:Type () in
    SymbolTracker.add_global tracker module_.mb_name.loc symbol;
    (* TODO: Determine if this should be type or namespace... *)
    let module_descriptor = default_descriptor ~name ~suffix:Type () in
    state.with_descriptor module_descriptor
    @@ fun () -> default_iterator.module_binding this module_
  in
  let iter = { default_iterator with value_binding; module_binding } in
  iter.structure iter structure
;;

let traverse document structure =
  let relative_path = document.relative_path in
  let relative_path = Filename.remove_extension relative_path in
  let descriptors =
    ref [ default_descriptor ~name:relative_path ~suffix:Namespace () ]
  in
  let state = IterState.init descriptors in
  let tracker = SymbolTracker.init () in
  find_symbols structure state tracker;
  SymbolLookup.init
    (SymbolTracker.get_globals tracker)
    (SymbolTracker.get_locals tracker)
;;

(* TODO: Create local symbol iterator... just adds to local iterator *)
