open Utils

(* ---------- Parsing tests ---------- *)

let%test "empty-prog" =
  expects_parse_err {| |}

let%test "missing-istrBloc" =
  expects_parse_err {|
    class Couleur(var coul: Integer) is {
      def Couleur (var coul: Integer) is {
        if this.coul < 0 then this.coul := 0;
        else if this.coul > 2 then this.coul := 0; else {}
      }
    }
  |}

let%test "wrong-decls" = 
  expects_parse_err {|
    {
      var null : Integer
    }
  |} &&
  expects_parse_err {|
    {
      next : Integer;
      is {}
    }
  |} &&
  expects_parse_err {|
    {
      x, y, z : Integer
    }
  |} && 
  expects_parse_err {|
    {
      var x : Integer 
      is { 
        x = 5;
      }
    }
  |} 
  
let%test "wrong-class-decl" =
  expects_parse_err {|
    class Point() {}
    {}
  |} &&
  expects_parse_err {|
    class Point is {}
    {}
  |} && 
  expects_parse_err {|
    class Point() extends is {}
    {}
  |} && 
  expects_parse_err {|
    class Point() extends None is
  |}

let%test "wrong-classbody" = 
  expects_parse_err {|
    class Point(x, y: Integer) is {
     def Point(var x, y: Integer, var name: String) 
      { this.index := Point.incr(); this.hasClone := 0; }
    }
    {}
  |} &&  
  expects_parse_err {|
    class Point(x, y: Integer) is {
     def static Point(var x, y: Integer, var name: String) is
      { this.index := Point.incr(); this.hasClone := 0; }
    }
    {}
  |} 

let%test "no-static-override" =
  let code = Printf.sprintf {|
      class Derived() is {
        def Derived() is {}
        def static test() is {}
      }
      class Base() extends Derived is {
        def Base(): Derived() is {}
        def static %s test() is {}
      }
      {}
    |}
  in expects_parse_err (code "override")
  && expects_ast (code "")

let%test "constructor-parameters-and-class-parameters-are-equal" =
  let code = Printf.sprintf {|
      class Point1(%s) is {
        def Point1(%s) is {}
      }
      {}
    |}
  in expects_syntax_err (code "s : String" "s : Integer")
  && expects_ast (code "s : String" "s : String")

let%test "constructor-calls-right-super-constructor-name-if-class-derived" =
  let code = Printf.sprintf {|
      class Base(var s: String, i1, i2: Integer) is {
        def Base(var s: String, i1, i2: Integer) is {}
      }
      class Derived() extends Base is {
        def Derived() : %s is {}
      }
      {}
    |}
  in expects_syntax_err (code {| Test("hello", 10, 20) |})
  && expects_ast (code {| Base("hello", 10, 20) |})

let%test "no-super-constructor-call-in-base-class" =
  let code = Printf.sprintf {|
      class Point1() is {
        def Point1() %s is {}
      }
      {}
    |}
  in expects_syntax_err (code " : Foo()")
  && expects_ast (code "")
