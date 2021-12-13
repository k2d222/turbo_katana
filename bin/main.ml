open Printf
open Libcompil
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Libcompil.Parser.MenhirInterpreter

let fast_parse (filename: string): Ast.prog option =
  let text, lexbuf = L.read filename in
  match Parser.prog Lexer.token lexbuf with
    | ast -> Some(ast)
    (* | exception Lexer.Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1 *)
    | exception Parser.Error -> None

let env_from_err checkpoint =
  match checkpoint with
    | I.HandlingError env -> env
    | _ -> assert false

(* [state_from_err checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state_from_err checkpoint: int =
  let env = env_from_err checkpoint in
  match I.top env with
    | Some (I.Element (s, _, _, _)) -> I.number s
    | None -> 0 (* initial state is usually 0 *)

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
    |> E.sanitize
    |> E.compress
    |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match I.get i (env_from_err checkpoint) with
    | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
    | None -> "???"

let diagnostic text checkpoint =
  try
    let message = ParserMessages.message (state_from_err checkpoint) in
    E.expand (get text checkpoint) message
  with
    | Not_found -> "<sorry, no diagnostics>"

let on_success ast: Ast.prog option =
  printf "on success\n";
  Some(ast)

let on_error text buffer (checkpoint: _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let indication = sprintf "Syntax error %s\n" (E.show (show text) buffer) in
  let message = diagnostic text checkpoint in
  eprintf "%s%s%s\n" location indication message;
  None

let table_parse (filename: string): Ast.prog option =
  let text, lexbuf = L.read filename in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  I.loop_handle on_success (on_error text buffer) supplier checkpoint

(* -------------------------------------------------------------------------- *)

let run ast = begin
  Ast.show_prog ast
  |> print_endline;
end

let () =
  let filename = Sys.argv.(1) in
  let res = fast_parse filename in
  match res with
    | Some(ast) -> run ast
    | None ->
      printf "Syntax error occured, running diagnostics...\n";
      let _ = table_parse filename in exit(1)
