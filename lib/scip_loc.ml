(* module T = struct *)
(*     type t = { title: string; isbn: string } *)
(*     let compare t1 t2 = *)
(*       let cmp_title = String.compare t1.title t2.title in *)
(*       if cmp_title <> 0 then cmp_title *)
(*       else String.compare t1.isbn t2.isbn *)
(*     let sexp_of_t t : Sexp.t = *)
(*       List [ Atom t.title; Atom t.isbn ] *)
(*   end *)
(*   include T *)
(*   include Comparator.Make(T) *)

(* type position = Lexing.position = { *)
(*   pos_fname : string; *)
(*   pos_lnum : int; *)
(*   pos_bol : int; *)
(*   pos_cnum : int; *)
(* } [@@deriving compare, sexp_of] *)
(* type t= Location.t =   { loc_start: position; loc_end: position; loc_ghost: bool } [@@deriving compare, sexp_of] *)

module ScipLoc = struct
  module T = struct
    type t =
      { filename : string
      ; start_line : int
      ; start_col : int
      ; end_line : int
      ; end_col : int
      }
    [@@deriving compare, sexp_of]

    (* ChatGPT might be a genius. *)
    type comparator_witness = ..

    let of_loc (p : Warnings.loc) : t =
      (* { loc_start = p.loc_start; loc_end = p.loc_end; loc_ghost = p.loc_ghost } *)
      { filename = p.loc_start.pos_fname
      ; start_line = p.loc_start.pos_lnum
      ; start_col = p.loc_start.pos_cnum
      ; end_line = p.loc_end.pos_lnum
      ; end_col = p.loc_end.pos_cnum
      }
    ;;
  end

  include T
  include Comparator.Make (T)
end

module ScipLocMap = Map.M (ScipLoc)
