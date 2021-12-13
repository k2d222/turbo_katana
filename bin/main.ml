open Libcompil
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try
    Parser.prog Lexer.token lexbuf
  with
    Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      failwith "syntax error"

let run_interprete ast =
  let res = Eval.run_prog ast in
  Printf.printf "résultat de l'évaluation : %i\n" res;
  Print.print ast

let run_compil ast =
  let res = Compil.compile ast in
  print_endline res;
  ()
  (* print_endline @@ Ast.show_progType ast *)

let () =
  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let ast = parse_with_error lexbuf in
  (* print_string "\n\n--- interprété ---\n\n";
  run_interprete ast;
  print_string "\n\n--- complilé ---\n\n"; *)
  run_compil ast;
  close_in file
