open Printf
open Libcompil
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let fast_parse (filename: string): Ast.prog option =
  let _text, lexbuf = L.read filename in
  match Parser.prog Lexer.token lexbuf with
  | ast -> Some(ast)
  | exception Parser.Error -> None

let env_from_err checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text env i =
  match I.get i env with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> "<???>"

let diagnostic text checkpoint =
  let env = env_from_err checkpoint in
  let state = I.current_state_number env in
  try
    let message = ParserMessages.message state in
    (E.expand (get text env) message)
  with
  | Not_found -> "<error happened in state " ^ string_of_int state ^ ">"

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

let () = begin
  let argc = Array.length Sys.argv in
  
  if argc <> 2 && (argc <> 4 || Sys.argv.(2) <> "-o")
  then failwith ("Usage: " ^ Sys.argv.(0) ^ " <file-to-compile> -o <output-file>");

  let filename = Sys.argv.(1) in
  let ast = fast_parse filename in

  match ast with
  | Some(ast) -> 
    let buf = if argc = 4 then open_out Sys.argv.(3) else stdout in
    Contextual.check_all ast;
    Compil.chan := buf;
    Compil.compile ast;
    if argc = 4 then close_out buf;
    exit 0;

  | None ->
    printf "Syntax error occured, running diagnostics...\n";
    ignore @@ table_parse filename;
    exit 1;
  
end