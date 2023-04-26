  module WithFunction = struct
(*       ^^^^^^^^^^^^ definition scip-ocaml opam . . simple_module/WithFunction# *)
    type t =
(*       ^ definition scip-ocaml opam . . simple_module/WithFunction#t# *)
      { x : int
(*      ^ definition scip-ocaml opam . . simple_module/WithFunction#t#x. *)
      ; y : int
(*      ^ definition scip-ocaml opam . . simple_module/WithFunction#t#y. *)
      }
  
    let read () = print_endline "read"
(*      ^^^^ definition scip-ocaml opam . . simple_module/WithFunction#read(). *)
    let init () = { x = 1; y = 2 }
(*      ^^^^ definition scip-ocaml opam . . simple_module/WithFunction#init(). *)
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
    let add t i = { t with x = t.x + i }
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#add(). *)
(*          ^ definition scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*            ^ definition scip-ocaml opam . . simple_module/WithFunction#add().(i) *)
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*                             ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*                               ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*                                   ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(i) *)
    let sub t i = { t with y = t.y - i }
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#sub(). *)
(*          ^ definition scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*            ^ definition scip-ocaml opam . . simple_module/WithFunction#sub().(i) *)
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
(*                             ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*                               ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
(*                                   ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(i) *)
  end
  
  let a = WithFunction.init ()
(*    ^ definition scip-ocaml opam . . simple_module/a. *)
(*        ^^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#init(). *)
  let b = WithFunction.add a 1
(*    ^ definition scip-ocaml opam . . simple_module/b. *)
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#add(). *)
(*                         ^ reference scip-ocaml opam . . simple_module/a. *)
  let c = WithFunction.sub b 2
(*    ^ definition scip-ocaml opam . . simple_module/c. *)
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#sub(). *)
(*                         ^ reference scip-ocaml opam . . simple_module/b. *)
  let _ = c
(*        ^ reference scip-ocaml opam . . simple_module/c. *)
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
  
  
