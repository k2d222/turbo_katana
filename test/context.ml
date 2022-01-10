open Utils
open Core

(* ---------- Contextual tests ---------- *)

let%test "minimal-prog" =
  expects_ast {| {} |}


(* ----------------------- Class Declarations -------------------------- *)

let%test "no-class-inherits-reserved" =
  let reserved = [ "Integer"; "String" ]
  in let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
      }
      class Test() extends %s is { 
        def Test() : %s() is {} 
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r r))
  && expects_ast (code "Base" "Base")

let%test "no-duplicate-class-declaration" =
  let code = Printf.sprintf {|
      class %s() is {
        def %s() is {}
      }

      class %s(i1 : Integer) is {
        def %s(i1 : Integer) is {}
      }

      {}
    |}
  in expects_ctx_err (code "Test" "Test" "Test" "Test")
  && expects_ast (code "Test1" "Test1" "Test2" "Test2")

let%test "no-reserved-class-name" =
  let reserved = [ "Integer"; "String"]
  in let code = Printf.sprintf {|
    class %s() is {
      def %s() is {}
    }
    {}
  |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r r))
  && expects_ast (code "Foo" "Foo")

let%test "no-duplicate-static-method-declaration" =
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
        def test3() : Integer is { result := 0; }
      }
      class Point1() extends Base is {
        def Point1() : Base() is {}
        def static test1() : Integer is { result := 0; }
        def static %s() : Integer is { result := 0; }
        def static %s() : String is { result := ""; }
      }
      {}
    |}
  in expects_ctx_err (code "test1" "test2")
  && expects_ctx_err (code "test2" "test1")
  && expects_ast (code "test2" "test3")
  && expects_ast (code "test3" "test2")

let%test "no-duplicate-instance-method-declaration" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def test1() : Integer is {result := 0;}
        def %s() : Integer is {result := 0;}
        def %s() : String is {result := "";}
      }
      {}
    |}
  in expects_ctx_err (code "test1" "test2")
  && expects_ctx_err (code "test2" "test1")
  && expects_ast (code "test2" "test3")

let%test "herited-class-exists" =
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
      }
  
      class Derived() extends %s is {
        def Derived() : %s() is {}
      }
      {}
    |}
  in expects_ctx_err (code "Test" "Test")
  && expects_ast (code "Base" "Base")

let%test "no-cycle-in-inheritance-graph" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
      }
      class Point2() extends Point3 is {
        def Point2() : Point3() is {}
      }
      class Point3() extends %s is {
        def Point3() : %s() is {}
      }
      {}
    |}
  in expects_ctx_err (code "Point2" "Point2")
  && expects_ast (code "Point1" "Point1")

(* ----------------------- Attributes -------------------------- *)

let%test "no-duplicate-instance-attribute-declaration" =
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
        var test3 : Integer
      }
      class Point1() extends Base is {
        def Point1() : Base() is {}
        var test1 : Integer
        var %s : Integer
        var %s : String
      }
      {}
    |}
  in expects_ctx_err (code "test1" "test2")
  && expects_ctx_err (code "test2" "test1")
  && expects_ast (code "test2" "test3")
  && expects_ast (code "test3" "test2")

let%test "no-duplicate-static-attribute-declaration" =
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
        var static test3 : Integer
      }
      class Point1() extends Base is {
        def Point1() : Base() is {}
        var static test1 : Integer
        var static %s : Integer
        var static %s : String
      }
      {}
    |}
  in expects_ctx_err (code "test1" "test2")
  && expects_ctx_err (code "test2" "test1")
  && expects_ast (code "test2" "test3")
  && expects_ast (code "test3" "test2")

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
  && expects_ast (code "foo")

(* ----------------------- Methods -------------------------- *)

let%test "no-reserved-keyword-in-method-params" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      class Test() is { 
        def Test() is {}
        def testMethod(%s : Integer) : Integer is { result := 0; }
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))
  && expects_ast (code "foo")

let%test "no-method-with-override-in-a-base-class" =
  let code = Printf.sprintf {|
      class Point() is {
        def Point() is {}
        def %s test() is {}
      }
      {}
    |}
  in expects_ctx_err (code "override")
  && expects_ast (code "")

let%test "override-methods-have-override-keyword" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def test() is {}
      }
      class Point2() extends Point1 is {
        def Point2() : Point1() is {}
        def %s test() is {}
      }
      {}
    |}
  in expects_ctx_err (code "")
  && expects_ast (code "override")

let%test "override-methods-match-overriden-method-signature" =
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
      }
      class Derived() extends Base is {
        def Derived() : Base() is {}
      }  
      class Point1() is {
        def Point1() is {}
        def test(i1, i2 : Integer, b : Base) is {}
      }
      class Point2() extends Point1 is {
        def Point2() : Point1() is {}
        def override test(%s) is {}
      }
      {}
    |}
  in expects_ctx_err (code "i1, i2 : Integer")
  && expects_ctx_err (code "i1, i2 : Integer, b : String")
  && expects_ctx_err (code "foo, bar : Integer, baz : Derived")
  && expects_ast (code "foo, bar : Integer, baz : Base")

(* TODO: non-void-code-paths-lead-to-assign-to-result *)

(* TODO: no-static-override *)

(* ----------------------- Constructors -------------------------- *)

let%test "no-reserved-keyword-in-constructor-params" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      class Test(%s : Integer) is { 
        def Test(%s : Integer) is {}
      }
      {}
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r r))
  && expects_ast (code "foo" "foo")

let%test "constructor-name-and-class-name-are-equal" =
  let code = Printf.sprintf {|
      class %s() is {
        def %s() is {}
      }
      {}
    |}
  in expects_ctx_err (code "Point1" "Point2")
  && expects_ast (code "Point1" "Point1")

let%test "constructor-parameters-and-class-parameters-are-equal" =
  let code = Printf.sprintf {|
      class Point1(%s) is {
        def Point1(%s) is {}
      }
      {}
    |}
  in expects_ctx_err (code "s : String" "s : Integer")
  && expects_ast (code "s : String" "s : String")

let%test "constructor-calls-right-super-constructor-if-class-derived" =
  let code = Printf.sprintf {|
      class Base(var s: String, i1, i2: Integer) is {
        def Base(var s: String, i1, i2: Integer) is {}
      }
      class Derived() extends Base is {
        def Derived() : %s is {}
      }
      {}
    |}
  in expects_ctx_err (code {| Test("hello", 10, 20) |})
  && expects_ctx_err (code {| Base("hello", 10) |})
  && expects_ast (code {| Base("hello", 10, 20) |})

(* TODO: no-super-constructor-call-in-base-class *)


(* ----------------------- Instructions -------------------------- *)

let%test "no-reserved-keyword-declared-in-block-instructions" = 
  let reserved = [ "this"; "super"; "result" ]
  in let code = Printf.sprintf {|
      {
        %s : Integer is {}
      }
    |}
  in List.for_all reserved ~f:(fun r -> expects_ctx_err (code r))
  && expects_ast (code "foo")

(* TODO: only-assign-to-idents-or-attribs *)

(* TODO: no-assign-to-this-or-super *)

let%test "no-undeclared-variable" =
  let code = Printf.sprintf {|
      {
        p1, p2 : Integer
        is
        %s := 51;
      }
    |}
  in expects_ctx_err (code "p3")
  && expects_ast (code "p2")
  
let%test "no-incompatible-assign-types" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
      }
      class Point2() is {
        def Point2() is {}
      }
      class Point3() extends Point2 is {
        def Point3() : Point2() is {}
      }
      {
        p1: Point1
        p2: Point2
        p3: Point3
        is
        %s := %s;
      }
    |}
  in expects_ctx_err (code "p1" "p2")
  && expects_ctx_err (code "p3" "p2")
  && expects_ast (code "p2" "p3")

(* TODO: ite-expression-type-is-integer *)

(* ----------------------- Expressions -------------------------- *)

  let%test "call-to-new-exists" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
      }
      {
        p : Point1
        is 
        p := new %s();
      }
    |}
  in expects_ctx_err (code "Point2")
  && expects_ast (code "Point1")

let%test "called-method-exists" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def test() is {}
      }
      {
        p : Point1
        is 
        p := new Point1();
        p.%s();
      }
    |}
  in expects_ctx_err (code "foo")
  && expects_ast (code "test")

let%test "called-method-params-are-compatible-with-declaration" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def test(i1, i2 : Integer, s: String) is {}
      }
      {
        p : Point1
        is 
        p := new Point1();
        p.test(%s);
      }
    |}
  in expects_ctx_err (code "10, 20")
  && expects_ctx_err (code "10, 20, 30")
  && expects_ast (code "10, 20, \"hello\"")

(* TODO: static-method-call-exists *)

(* TODO: static-method-call-params-compatible *)

let%test "params-in-new-call-compatible-with-constructor" =
  let code = Printf.sprintf {|
      class Point1(var i1, i2 : Integer, s: String) is {
        def Point1(var i1, i2 : Integer, s: String) is {}
        def test() is {}
      }
      {
        p : Point1
        is 
        p := new Point1(%s);
      }
    |}
  in expects_ctx_err (code "10, 20")
  && expects_ctx_err (code "10, 20, 30")
  && expects_ast (code "10, 20, \"hello\"")

(* TODO: numeric-operators-used-on-integers *)

(* TODO: string-operators-used-on-strings *)

let%test "identifiers-are-in-scope" =
  let code = Printf.sprintf {|
      {
        p1 : Integer
        is 
        {
          p2 : Integer
          is 
          p2 := %s;
        }
        p1 := %s;
      }
    |}
  in expects_ctx_err (code "p1" "p2")
  && expects_ast (code "p1" "10")

let%test "attributes-exist" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        var attr : Integer
      }
      {
        p : Point1
        is 
        p := new Point1();
        p.%s := 5;
      }
    |}
  in expects_ctx_err (code "foo")
  && expects_ast (code "attr")
  
(* TODO: static-attributes-exist *)

(* ----------------------------------------------- *)



