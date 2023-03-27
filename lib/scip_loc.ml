module ScipLoc = struct
  type t = Location.t

  (* TODO: This seems kind of stupid... *)
  let of_loc (p : Warnings.loc) : t =
    { loc_start = p.loc_start; loc_end = p.loc_end; loc_ghost = p.loc_ghost }
  ;;

  let hash (this : t) =
    let start_ = this.loc_start in
    let end_ = this.loc_end in
    Format.sprintf
      "%s:%d:%d-%d:%d"
      start_.pos_fname
      start_.pos_lnum
      start_.pos_cnum
      end_.pos_lnum
      end_.pos_cnum
  ;;

  let compare a b = Stdlib.compare (hash a) (hash b)
end

module ScipLocMap = Map.Make (ScipLoc)
