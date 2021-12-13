%{
open Ast
open Print

type classBodyElt =
  | Method of Ast.methodDecl
  | StaticAttrib of Ast.param
  | InstAttrib of Ast.param

let partition_of_body_elts l =

  let rec r_partition l (lm, ls, li) =
    match l with
    | [] -> (lm, ls, li)
    | e::r -> (match e with
      | Method m -> (m::lm, ls, li)
      | StaticAttrib s -> (lm, s::ls, li)
      | InstAttrib i -> (lm, ls, i::li))

  in r_partition l ([], [], [])
%}

(* Class Keywords *)
%token CLASS
%token EXTENDS
%token SUPER
%token NEW

(* Function Keywords *)
%token IS
%token VAR
%token DEF
%token STATIC
%token OVERRIDE
%token RETURN

(* Flow Control Keywords *)
%token IF
%token THEN
%token ELSE

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LCURLY RCURLY
%token SEMICOLON COLON COMMA DOT
%token ASSIGN
%token EOF

// %right ELSE
%left PLUS MINUS
%left TIMES DIV

%type <Ast.prog> prog

%start prog
%%

prog:
  ld = list(classDecl) i = instr EOF { { decls=ld; instr=i } }

classDecl:
  CLASS cname = ID params = paramList ext = extends IS body = classBody {
    { name=cname; ctorParams=params; body=body; superclass=ext }
  }

classBody:
  LCURLY l = list(classBodyElement) RCURLY {
    let (lm, ls, li) = partition_of_body_elts l
    in { methods=lm; staticAttrs=ls; instAttrs=li }
  }

classBodyElement:
  | md = methodDecl { md }
  | attr = attrDecl { attr }

methodDecl:
  | DEF static = boption(STATIC) override = boption(OVERRIDE) name = ID params = paramList COLON retType = ID IS b = instrBlock {
     Method({ name=name; params=params; retType=retType; body=b })
  }

attrDecl:
  | VAR static = boption(STATIC) name = ID COLON clName = ID SEMICOLON {
    let p = { name=name; className=clName } in
    if static then StaticAttrib(p) else InstAttrib(p)
  }


paramList:
  LPAREN lp = separated_list(COMMA, param) RPAREN { List.flatten lp }

param:
  varopt = VAR? names = separated_nonempty_list(COMMA, ID) COLON clname = ID { List.map (fun name -> { name=name; className=clname }) names }

extends:
  | EXTENDS id = ID { Some(id) }
  | { None }

instrBlock:
  LCURLY li = separated_list(SEMICOLON, instr) RCURLY { Block([]) }

instr:
  | b = instrBlock { b }
  | e = expr SEMICOLON { Expr(e) }
  | id = ID ASSIGN e = expr SEMICOLON { Assign(id, e) }

expr:
  | c = CSTE { Cste(c) }
  | id = ID { Id(id) }
  | e = expr DOT name = ID { Select(e, name) }
