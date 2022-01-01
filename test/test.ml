open Libcompil
open Core

let parse_file path =
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ast = Parser.prog Lexer.token lexbuf in
  In_channel.close file;
  ast

let%test_unit "ex1" =
  let _ast = parse_file "../progs/ex1.kat"
  in ()

let%test_unit "all" =
  ()