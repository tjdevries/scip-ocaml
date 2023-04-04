module WithFunction = struct
  type t =
    { x : int
    ; y : int
    }

  let read () = print_endline "read"
  let init () = { x = 1; y = 2 }
  let add t i = { t with x = t.x + i }
  let sub t i = { t with y = t.y - i }
end

let a = WithFunction.init ()
let b = WithFunction.add a 1
let c = WithFunction.sub b 2
let _ = c
let read () = "read"
let _ = read ()

let () =
  let _ = WithFunction.read () in
  print_endline "done"
;;

let () =
  let open WithFunction in
  let _ = read () in
  print_endline "done"
;;

