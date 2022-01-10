open Libcompil
open Core

let parse_file path =
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ast = Parser.prog Lexer.token lexbuf in
  In_channel.close file;
  ast

let parse_str str =
  let lexbuf = Lexing.from_string str
  in Parser.prog Lexer.token lexbuf

let expects_err str =
  try Contextual.check_all @@ parse_str str; false
  with Failure _ -> true

let expects_parse_err str =
  try Contextual.check_all @@ parse_str str; false
  with Parser.Error -> true

let expects_ctx_err str =
  try Contextual.check_all @@ parse_str str; false
  with Contextual.Contextual_error _ -> true

let expects_ast str =
  Contextual.check_all @@ parse_str str; true

(* ------------------------------------- *)

let%test_unit "ex1-parse" =
  ignore @@ parse_file "../progs/ex1.kat"

(* ---------- Lexical tests ---------- *)

let%test "invalid-token" =
  expects_err {| # |}

(* ---------- Parsing tests ---------- *)

let%test "empty-prog" =
  expects_parse_err {| |}

(* ---------- Contextual tests ---------- *)

let%test "minimal-prog" =
  expects_ast {| {} |}

let%test "no-class-inherits-reserved" =
  let reserved = [ "Integer"; "String"; "_Void" ]
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

(* ---------- Compilation tests ---------- *)


(* ---------- Execution tests ---------- *)
