  module WithFunction = struct
(*       ^^^^^^^^^^^^ definition scip-ocaml opam . . simple_module/WithFunction# *)
    type t =
(*       ^ definition scip-ocaml opam . . simple_module/WithFunction#t# *)
      { x : int
(*      ^ definition scip-ocaml opam . . simple_module/WithFunction#t/x. *)
      ; y : int
(*      ^ definition scip-ocaml opam . . simple_module/WithFunction#t/y. *)
      }
  
    let read () = print_endline "read"
(*      ^^^^ definition scip-ocaml opam . . simple_module/WithFunction#read(). *)
    let init () = { x = 1; y = 2 }
(*      ^^^^ definition scip-ocaml opam . . simple_module/WithFunction#init(). *)
    let add t i = { t with x = t.x + i }
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#add(). *)
    let sub t i = { t with y = t.y - i }
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#sub(). *)
  end
  
  let a = WithFunction.init ()
(*        ^^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#init(). *)
  let b = WithFunction.add a 1
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#add(). *)
  let c = WithFunction.sub b 2
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#sub(). *)
  let _ = c
  let read () = "read"
(*    ^^^^ definition scip-ocaml opam . . simple_module/read(). *)
  let _ = read ()
(*        ^^^^ reference scip-ocaml opam . . simple_module/read(). *)
  
  let () =
    let _ = WithFunction.read () in
(*          ^^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#read(). *)
    print_endline "done"
  ;;
  
  let () =
    let open WithFunction in
    let _ = read () in
(*          ^^^^ reference scip-ocaml opam . . simple_module/WithFunction#read(). *)
    print_endline "done"
  ;;
  
  
