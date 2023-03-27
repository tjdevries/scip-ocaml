module ScipSymbol : sig
  type t = Scip_proto.Scip_types.symbol

  (* Create a new formatted local string *)
  val new_local : int -> string

  (* Convert string to scip symbol *)
  val of_string : string -> (t, string) result

  (* Convert scip symbol to string *)
  val to_string : t -> string
end
