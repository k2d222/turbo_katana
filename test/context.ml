open Utils
open Core

(* ---------- Contextual tests ---------- *)

let%test "minimal-prog" =
  expects_ast {| {} |}

let%test "no-class-inherits-reserved" =
  let reserved = [ "Integer"; "String"; "Void" ]
  in let code = Printf.sprintf {|
      class Test() extends %s is { def Test() is {} }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))

let%test "no-static-override" =
  expects_ctx_err {|
      class Test() is {
        def Test() is {}
        def static override foo() is {}
      }
      {}
    |}
