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
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*      ^^^^ definition scip-ocaml opam . . simple_module/WithFunction#init(). *)
    let add t i = { t with x = t.x + i }
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*                                   ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(i) *)
(*                             ^ reference scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*                               ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#x. *)
(*            ^ definition scip-ocaml opam . . simple_module/WithFunction#add().(i) *)
(*          ^ definition scip-ocaml opam . . simple_module/WithFunction#add().(t) *)
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#add(). *)
    let sub t i = { t with y = t.y - i }
(*                  ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*                                   ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(i) *)
(*                             ^ reference scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*                               ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
(*                         ^ reference scip-ocaml opam . . simple_module/WithFunction#t#y. *)
(*            ^ definition scip-ocaml opam . . simple_module/WithFunction#sub().(i) *)
(*          ^ definition scip-ocaml opam . . simple_module/WithFunction#sub().(t) *)
(*      ^^^ definition scip-ocaml opam . . simple_module/WithFunction#sub(). *)
  end
  
  let a = WithFunction.init ()
(*        ^^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#init(). *)
(*    ^ definition scip-ocaml opam . . simple_module/a. *)
  let b = WithFunction.add a 1
(*                         ^ reference scip-ocaml opam . . simple_module/a. *)
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#add(). *)
(*    ^ definition scip-ocaml opam . . simple_module/b. *)
  let c = WithFunction.sub b 2
(*                         ^ reference scip-ocaml opam . . simple_module/b. *)
(*        ^^^^^^^^^^^^^^^^ reference scip-ocaml opam . . simple_module/WithFunction#sub(). *)
(*    ^ definition scip-ocaml opam . . simple_module/c. *)
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
  
  
