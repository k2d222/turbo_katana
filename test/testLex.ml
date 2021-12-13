open Libcompil
open Libcompil.Parser

let output token =
    match token with
      | TIMES -> " * "
      | THEN -> " THEN "
      | SEMICOLON -> ";\n"
      | RPAREN -> ")"
      | RELOP op -> " " ^ Misc.string_of_relop op ^ " "
      | PLUS -> " + "
      | MINUS -> " - "
      | LPAREN -> "("
      | IF -> " IF "
      | ID id -> id
      | END -> "\nEND\n"
      | ELSE -> " ELSE "
      | DIV -> " / "
      | CSTE v -> string_of_int v
      | BEGIN -> "\nBEGIN\n  "
      | ASSIGN -> " := "
      | _ -> "**Unexpected token in testLex**"

let () =
  if Array.length Sys.argv = 1 then
    failwith "usage: textLex nom-de-fichier"
  else
    begin
      let file = open_in Sys.argv.(1) in
      let lexbuf = Lexing.from_channel file in
      let rec process () =
        match Lexer.token lexbuf with
          | EOF -> close_in file
          | tok -> print_string (output tok); process ()
      in process ();
    end
