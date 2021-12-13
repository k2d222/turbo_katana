{
open Ast
open Parser
open Lexing
open Optmanip

exception Eof

let keyword_table = Hashtbl.create 16

let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        (* Class Keywords *)
        "class", CLASS;
        "extends", EXTENDS;
        "super", SUPER;
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
  | (lettre | '_') identchar* as id  {
      Hashtbl.find_opt keyword_table id
      |> get_or (ID(id))
    }
  | chiffre+ as cst { CSTE (int_of_string cst) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
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
  | "." { COMMA }
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
