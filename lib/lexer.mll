{
open Ast
open Parser
open Lexing
exception Eof

let keyword_table = Hashtbl.create 16

let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        "class", CLASS;
        "extends", EXTENDS;
        "is", IS;
        "var", VAR;
        "def", DEF;
        "static", STATIC;
        "override", OVERRIDE;
        "if", OVERRIDE;
        "override", OVERRIDE;
      ]
}

let lettre = ['A'-'Z' 'a'-'z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )

rule
  token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "/*" { comment lexbuf }
  | lettre LC* as id  {
      match Hashtbl.find_opt keyword_table id with
        | Some(k) -> k
        | None -> ID(id)
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
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
  | _ as t { failwith (Printf.sprintf "undefined character: '%c'" t) }
and
comment = parse
  | "*/" { token lexbuf }
  | _ { comment lexbuf }
