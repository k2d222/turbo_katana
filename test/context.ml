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
        var %s : Integer
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

let%test "no-duplicate-class-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
      }

      class Point1(i1 : Integer) is {
        def Point1(i1 : Integer) is {}
      }

      {}
    |}

let%test "no-reserved-class-name" =
  let reserved = [ "Integer"; "String"]
  in let code = Printf.sprintf {|
    class %s() is {
      def %s() is {}
    }
    {}
  |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r r))

let%test "no-duplicate-static-attribute-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        var static static1 : Integer
        var static static1 : String
      }
      {}
    |}


let%test "no-duplicate-static-method-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        def static static1() : Integer is {return 0;}
        def static static1() : String is {return "";}
      }
      {}
    |}

let%test "no-duplicate-instance-attribute-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        var static1 : Integer
        var static1 : String
      }
      {}
    |}

let%test "no-duplicate-instance-method-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        def static1() : Integer is {return 0;}
        def static1() : String is {return "";}
      }
      {}
    |}

let%test "herited-class-exists" =
  expects_ctx_err {|
      class Point1() extends Point2 is {
        def Point1() : Point2() is {}
      }
      {}
    |}

let%test "no-cycle-in-inheritance-graph" =
  expects_ctx_err {|
      class Point1() extends Point2 is {
        def Point1() : Point2() is {}
      }
      class Point2() extends Point1 is {
        def Point2() : Point1() is {}
      }
      {}
    |}

let%test "no-method-with-override-in-a-base-class" =
  expects_ctx_err {|
      class Point() is {
        def Point() is {}
        def override test() is {}
      }
      {}
    |}

let%test "override-methods-have-the-override-keyword" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        def test() is {}
      }
      class Point2() extends Point1 is {
        def Point2() : Point1() is {}
        def test() is {}
      }
      {}
    |}

let%test "override-methods-match-the-overriden-method-signature" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        def test(i : Integer) is {}
      }
      class Point2() extends Point1 is {
        def Point2() : Point1() is {}
        def override test(i1, i2 : Integer) is {}
      }
      {}
    |}
    
let%test "no-return-instruction-in-constructor" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {
          return 0;
        }
      }
      {}
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

let%test "constructor-name-and-class-name-are-equal" =
  expects_ctx_err {|
      class Point1() is {
        def Point2() is {}
      }
      {}
    |}

let%test "constructor-parameters-and-class-parameters-are-equal" =
  expects_ctx_err {|
      class Point1(i : Integer) is {
        def Point1(i : String) is {}
      }
      {}
    |}


let%test "no-reserved-keyword-declared-in-Block-instructions" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      {
        %s : Integer is {}
      }
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))


let%test "called-method-exists" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
      }
      {p : Point1
      is 
      p.test();}
    |}

let%test "called-method-exists" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
      }
      {p : Point1
      is 
      p.test();}
    |}

let%test "called-method-params-are-compatible-with-declaration" =
  expects_ctx_err {|
      class Point1() is {
        def Point1() is {}
        def test(i : Integer) is {}
      }
      {p : Point1
      is 
      p.test();}
    |}