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
      var null : Integer;
    }
  |} &&
  expects_parse_err {|
    {
      next : Integer;
    }
  |} &&
  expects_parse_err {|
    {
      var x y z : Integer;
    }
  |}

let%test "wrong-class-decl" =
  expects_parse_err {|
    class Point() {}
    {}
  |} &&
  expects_parse_err {|
    class Point IS {}
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
      def 
    }
    {}
  |}  
