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

let expects_err f = 
  try ignore @@ f (); false 
  with Failure _ -> true

let expects_parse_err f = 
  try ignore @@ f (); false 
  with Parser.Error -> true

let expects_ctx_err f = 
  try ignore @@ f (); false 
  with Contextual.Contextual_error _ -> true

let expects_ast f =
  try ignore @@ f (); true with Parser.Error -> false  

(* ------------------------------------- *)


let%test "ex1-parse" =
  expects_ast @@ (fun () ->
    parse_file "../progs/ex1.kat"
  )
  
(* ---------- Lexical tests ---------- *)

let%test "invalid-token" =
  expects_err @@ (fun () -> 
    parse_str {| # |}
  )

(* ---------- Parsing tests ---------- *)

let%test "empty-prog" =
  expects_parse_err @@ (fun () -> 
    parse_str {| |}
  )

(* ---------- Contextual tests ---------- *)

let%test "minimal-prog" =
  expects_ast @@ (fun () -> 
    parse_str {| {} |}
  )

let%test "class-inherits-string" =
  expects_ctx_err @@ (fun () -> 
    Contextual.check_all @@ parse_str {|
      class Test() extends String is {
        def Test() is {}
      }
      {}
    |}
  )
  
(* ---------- Compilation tests ---------- *)


(* ---------- Execution tests ---------- *)