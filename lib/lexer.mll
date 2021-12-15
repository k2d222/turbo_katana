{
open Ast
open Parser
open Lexing
open Optmanip

exception SyntaxError of string

let keyword_table = Hashtbl.create 16

let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        (* Class Keywords *)
        "class", CLASS;
        "extends", EXTENDS;
        "new", NEW;

        (* Function Keywords *)
        "is", IS;
        "var", VAR;
        "def", DEF;
        "static", STATIC;
        "override", OVERRIDE;
        "return", RETURN;

        (* Flow Control Keywords *)
        "if", IF;
        "then", THEN;
        "else", ELSE;

        (* WTF ‽ *)
      ]
}



let lettre = ['A'-'Z' 'a'-'z']
let chiffre = ['0'-'9']
let identchar = lettre | chiffre | ['_']

rule
  token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "/*" { comment lexbuf }
  | (['A'-'Z'] identchar*) as id  { CLASSNAME(id) }
  | (['a'-'z'] identchar*) as id  {
      Hashtbl.find_opt keyword_table id
      |> get_or (ID(id))
    }
  | chiffre+ as cst { CSTE (int_of_string cst) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "&" { STRCAT }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | ">" { RELOP(Gt) }
  | "<" { RELOP(Lt) }
  | ">=" { RELOP(Ge) }
  | "<=" { RELOP(Le) }
  | "=" { RELOP(Eq) }
  | "<>" { RELOP(Neq) }
  | ":=" { ASSIGN }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | "," { COMMA }
  | "." { DOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | eof { EOF }
  | _ as t { failwith (Printf.sprintf "undefined character: '%c'" t) }
and
comment = parse
  | "*/" { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
and

read_string buf = parse
  | '"'       { STRLIT (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
