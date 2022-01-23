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
  try ignore @@ parse_str str; false
  with Failure _ -> true

let expects_parse_err str =
  try ignore @@ parse_str str; false
  with Parser.Error -> true

let expects_ctx_err str =
  try Contextual.check_all @@ parse_str str; false
  with Contextual.Contextual_error _ -> true

let expects_syntax_err str =
  try ignore @@ parse_str str; false
  with Ast.Syntax_error _ -> true

let expects_ast str =
  Contextual.check_all @@ parse_str str; true

let file_parse_err file = 
  try ignore @@ parse_file file; false
  with Parser.Error -> true

let file_ctx_err file = 
  try Contextual.check_all @@ parse_file file; false
  with Contextual.Contextual_error _ -> true 

let file_ast file =
  Contextual.check_all @@ parse_file file; true
