%{
open Ast
open Print
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token SEMICOLON
%token ASSIGN
%token BEGIN END
%token IF THEN ELSE
%token EOF

%right ELSE
%left PLUS MINUS
%left TIMES DIV

%type <Ast.progType> prog
%type <Ast.expType> expr
%type <Ast.declType> decl
%type <Ast.compType> comp


%start prog
%%

prog:
 | ld = list(decl) BEGIN e = expr END EOF { { decls=ld; expr=e } }

comp:
 | lhs = expr op = RELOP rhs = expr { Comp(op, lhs, rhs) }

expr:
  | x = ID { Id x }
  | v = CSTE { Cste v }
  | lhs = expr PLUS rhs = expr { Plus(lhs, rhs) }
  | lhs = expr MINUS rhs = expr { Minus(lhs, rhs) }
  | lhs = expr TIMES rhs = expr { Times(lhs, rhs) }
  | lhs = expr DIV rhs = expr { Div(lhs, rhs) }
  | MINUS e = expr { UMinus(e) }
  | PLUS e = expr { e }
  | IF i = comp THEN t = expr ELSE e = expr { Ite(i, t, e) }
  | e = delimited(LPAREN, expr, RPAREN) { e }

decl:
  | i = ID ASSIGN e = expr SEMICOLON { { lhs=i; rhs=e } }
