type t = int32 list [@@deriving sexp]

(* TODO: Handle different length ranges *)
let compare a b = compare a b

let of_vec v =
  match List.length v with
  | 3 -> v
  | 4 -> v
  | _ -> assert false
;;

let of_loc (loc : Warnings.loc) : t =
  let start_ = loc.loc_start in
  let finish_ = loc.loc_end in
  [ Int32.of_int_exn (start_.pos_lnum - 1)
  ; Int32.of_int_exn (start_.pos_cnum - start_.pos_bol)
  ; Int32.of_int_exn (finish_.pos_lnum - 1)
  ; Int32.of_int_exn (finish_.pos_cnum - finish_.pos_bol)
  ]
;;

let to_list this : int32 list = this
let to_string this = Fmt.str "%s" (Sexp.to_string_hum (sexp_of_t this))
