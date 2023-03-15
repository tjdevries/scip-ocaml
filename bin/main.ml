open Scip_ocaml.Scip_pp

let () = print_endline "Hello, World!"

let () =
  let x = Scip_ocaml.Scip_types.default_index () in
  Format.printf "x = %a@." pp_index x
