open Scip_loc
open Scip_proto.Scip_types
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
    ; globals : string ScipLocMap.t
    ; locals : string ScipLocMap.t
    }

  let init document globals locals = { path = document.relative_path; globals; locals }

  let lookup this loc =
    let loc = ScipLoc.of_loc loc in
    match Map.find this.globals loc with
    | Some symbol -> Some symbol
    | None -> Map.find this.locals loc
  ;;
end

type string_to_loc = string ScipLocMap.t

(* type string_map = Base.String.Map; *)
type int_to_string = int Map.M(String).t

(*  string -> string -> ScipLoc *)
(* string -> ScipLocal = ScipLocMap.t *)
type string_to_scip_map = string ScipLocMap.t

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
      | Texp_function _ -> Some (make_symbol ~descriptors ~name ~suffix:Method ())
      | Texp_constant _ -> Some (make_symbol ~descriptors ~name ~suffix:Term ())
      | _ ->
        Caml.Format.printf "  -> unknown expr: %s@." name;
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
  let iter = { default_iterator with value_binding; module_binding } in
  iter.structure iter structure
;;

let traverse document structure =
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
