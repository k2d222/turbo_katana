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

let%test "non-void-code-paths-lead-to-assign-to-result" =
  let template = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def test(): Integer is { %s } /* in methods */
        def static test2(): Integer is { %s } /* in static methods */
      }
      {}
    |}
  in let t1 = template "result := 0;"
  in let t2 = (Fn.flip template) "result := 0;"
  in let tests_err = [
      {| if 10 then result := 10; else {} |};
      {| if 10 then {} else result := 20; |};
      {| foo: Integer is foo := 10; |};
      {| return; result := 10; |};
    ]
  in let tests_ok = [
          {| if 10 then result := 10; else result := 20; |};
          {| foo: Integer is result := 10; |};
          {| result:= 10; return; |};
        ]
  in tests_err |> List.map ~f:t1 |> List.for_all ~f:expects_ctx_err
  && tests_err |> List.map ~f:t2 |> List.for_all ~f:expects_ctx_err
  && tests_ok |> List.map ~f:t1 |> List.for_all ~f:expects_ast
  && tests_ok |> List.map ~f:t2 |> List.for_all ~f:expects_ast

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

let%test "constructor-calls-right-super-params-constructor-if-class-derived" =
  let code = Printf.sprintf {|
      class Base(var s: String, i1, i2: Integer) is {
        def Base(var s: String, i1, i2: Integer) is {}
      }
      class Derived() extends Base is {
        def Derived() : %s is {}
      }
      {}
    |}
  in expects_ctx_err (code {| Base("hello", 10) |})
  && expects_ctx_err (code {| Base("hello", 10, "world") |})
  && expects_ast (code {| Base("hello", 10, 20) |})

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

let%test "only-assign-to-idents-or-attribs" =
  let code = Printf.sprintf {|
      class Point1(var s1: String) is {
        def Point1(var s1: String) is {}
        var i1: Integer
        var static i2: Integer
        def foo(): Integer := 10
      }
      {
        s: String
        i, j: Integer
        p: Point1
        is %s := %s;
      }
    |}
  in let ok = [
      ("s", "\"hello\"");
      ("i", "10");
      ("Point1.i2", "10");
      ("p.i1", "10");
      ("p.s1", "\"hello\"");
      ("new Point1(\"hello\").i1", "10");
    ]
  in let err = [
      ("\"foo\"", "\"hello\"");
      ("20", "10");
      ("20", "10");
      ("p.foo()", "10");
    ]  
  in err |> List.for_all ~f:(fun (a, b) -> expects_ctx_err (code a b))
  && ok |> List.for_all ~f:(fun (a, b) -> expects_ast (code a b))

let%test "no-assign-to-this-or-super" = 
  let code = Printf.sprintf {|
      class Base() is {
        def Base() is {}
      }
      class Derived() extends Base is {
        def Derived(): Base() is {}
        def test() is {
          b: Base
          d: Derived
          is %s := %s;
        }
      }
      {}
    |}
  in expects_ctx_err (code "this" "new Derived()")
  && expects_ctx_err (code "super" "new Base()")
  && expects_ast (code "d" "new Derived()")
  && expects_ast (code "b" "new Base()")

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

let%test "ite-expression-type-is-integer" =
  let code = Printf.sprintf {|
      {
        if %s then {} else {}
      }
    |}
  in expects_ctx_err (code "\"hello\"")
  && expects_ast (code "10")

let%test "cannot-cast-Integer-to-String" =
  let code = Printf.sprintf {|
      {
        %s;
      }
    |}
  in expects_ctx_err (code "(String 1)")
  && expects_ast (code "(String \"hello\")")

let%test "cannot-cast-from-class-to-subclass" =
  let code = Printf.sprintf {|
      class Point() is {
        def Point() is {}
      }
      class SubPoint() extends Point is {
        def SubPoint() : Point() is {}
      }
      {
        p : Point
        sp : SubPoint
        is
        %s;
      }
    |}
  in expects_ctx_err (code "(SubPoint p)")
  && expects_ast (code "(Point sp)")

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

let%test "static-method-call-exists" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def static test1() is {}
      }
      {
        Point1.%s();
      }
    |}
  in expects_ctx_err (code "test2")
  && expects_ast (code "test1")

let%test "static-method-call-params-compatible" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        def static test1(i1, i2: Integer, b: Base, d: Derived) is {}
      }
      class Base() is {
        def Base() is {}
      }
      class Derived() extends Base is {
        def Derived(): Base() is {}
      }
      {
        Point1.test1(%s);
      }
    |}
  in expects_ctx_err (code "10, 20, new Base()")
  && expects_ctx_err (code "10, 20, new Base(), new Base()")
  && expects_ast (code "10, 20, new Base(), new Derived()")
  && expects_ast (code "10, 20, new Derived(), new Derived()")

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

let%test "operators-used-on-right-type" =
  let code = Printf.sprintf {|
      { %s; }
    |}
  in let ops = [ "+"; "-"; "*"; "/"; "="; "<>"; "<"; ">"; ">="; "<=" ]
  in ops |> List.for_all ~f:(fun op -> expects_ctx_err (code ("\"hello\" " ^ op ^ " 10")))
  && expects_ctx_err (code "10 & \"hello\"")
  && ops |> List.for_all ~f:(fun op -> expects_ast (code ("10" ^ op ^ "10")))
  && expects_ast (code "\"hello\" & \"world\"")

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
  
let%test "static-attributes-exist" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() is {}
        var static attr : Integer
      }
      {
        Point1.%s := 5;
      }
    |}
  in expects_ctx_err (code "foo")
  && expects_ast (code "attr")

(* ----------------------------------------------- *)



