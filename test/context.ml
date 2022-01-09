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

let%test "underclared_variable" =
  expects_ctx_err {|
      {p1 := 51;}
    |}
  
let%test "unmatched-type" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
      }

      class Point2() is {
        def Point2() is {}
      }

      {
        p1: Point1
        p2: Point2
        is
        p1 := p2;
      }
    |}

let%test "no-reserved-keyword-in-constructor-params" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      class Test(%s : Integer) is { 
        def Test(%s : Integer) is {}
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r r))

let%test "no-reserved-keyword-in-method-params" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      class Test() is { 
        def Test() is {}
        def testMethod(%s : Integer) : Integer is {}
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))

let%test "no-reserved-keyword-in-attributes" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      class Test() is { 
        def Test() is {}
        var %s : Integer;
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))

let%test "no-reserved-keyword-in-instructions" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      {%s : Integer is {}}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))

