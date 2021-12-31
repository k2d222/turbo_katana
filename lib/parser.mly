%{
open Ast

type classBodyElt =
  | Method of Ast.methodDecl
  | Ctor of Ast.ctorDecl
  | StaticAttrib of Ast.param list
  | InstAttrib of Ast.param list

let split_body_elts l =

  let rec r_partition l (lm, lc, ls, li) =
    match l with
    | [] -> (lm, lc, ls, li)
    | e::r ->
      let res =  (match e with
        | Method m -> (m::lm, lc, ls, li)
        | Ctor c -> (lm, c::lc, ls, li)
        | StaticAttrib s -> (lm, lc, s@ls, li)
        | InstAttrib i -> (lm, lc, ls, i@li))
      in r_partition r res

  in r_partition l ([], [], [], [])
%}

(* Class Keywords *)
%token <string> CLASSNAME
%token CLASS EXTENDS NEW

(* Function Keywords *)
%token IS
%token VAR
%token DEF
%token STATIC
%token OVERRIDE
%token RETURN

(* Flow Control Keywords *)
%token IF THEN ELSE

%token <string> ID
%token <int> CSTE
%token <string> STRLIT
%token <Ast.numBinOp> RELOP
%token PLUS MINUS TIMES DIV
%token STRCAT
%token LPAREN RPAREN
%token LCURLY RCURLY
%token SEMICOLON COLON COMMA DOT
%token ASSIGN
%token EOF

%left RELOP
%left PLUS MINUS
%left TIMES DIV
%left STRCAT
%left DOT

%type <Ast.prog> prog

%start prog
%%

prog:
  decls = list(classDecl) instr = instrBlock EOF { { decls; instr } }

classDecl:
  CLASS name = CLASSNAME ctorParams = ctorParamList superclass = extends IS body = classBody {
    { name; ctorParams; body; superclass }
  }

classBody:
  LCURLY l = list(classBodyElement) RCURLY {
    let (lm, lc, ls, li) = split_body_elts l
    in let ctor = List.hd lc (* TODO: make sure there is only one ctor *)
    in { methods=lm; ctor; staticAttrs=ls; instAttrs=li }
  }

classBodyElement:
  | md = methodDecl { md }
  | cd = ctorDecl { cd }
  | attr = attrDecl { attr }

methodDecl:
  | DEF static = boption(STATIC) override = boption(OVERRIDE) name = ID params = paramList retType = option(preceded(COLON, CLASSNAME)) IS body = instrBlock {
     Method({ name; static; override; params; retType; body;  })
  }
  | DEF static = boption(STATIC) override = boption(OVERRIDE) name = ID params = paramList COLON retType = CLASSNAME ASSIGN e = expr {
    Method({ name; static; override; params; retType=Some(retType); body=Return(e); })
  }

ctorDecl:
  | DEF name = CLASSNAME params = ctorParamList IS body = instrBlock {
     Ctor({ name; params; superCall=None; body;  })
  }
  | DEF name = CLASSNAME params = ctorParamList COLON super = CLASSNAME lsuper = superList IS b = instrBlock {
    Ctor({ name; params; superCall=Some(super, lsuper); body=b; })
  }


attrDecl:
  | VAR static = boption(STATIC) lname = separated_list(COMMA, ID) COLON className = CLASSNAME {
    let p = List.map (fun name -> { name; className }) lname in
    if static then StaticAttrib(p) else InstAttrib(p)
  }

superList:
  LPAREN le = separated_list(COMMA, expr) RPAREN { le }

paramList:
  LPAREN lp = separated_list(COMMA, param) RPAREN { List.flatten lp }

ctorParamList:
  LPAREN lp = separated_list(COMMA, ctorParam) RPAREN { List.flatten lp }

ctorParam:
  isMember = boption(VAR) names = separated_nonempty_list(COMMA, ID) COLON className = CLASSNAME
  { List.map (fun name -> { isMember; name; className }) names }

extends:
  | EXTENDS id = CLASSNAME { Some(id) }
  | { None }

param:
  names = separated_nonempty_list(COMMA, ID) COLON className = CLASSNAME { List.map (fun name -> { name; className }) names }

instrBlock:
  | LCURLY li = list(instr) RCURLY { Block([], li) }
  | LCURLY lvar = list(param) IS li = nonempty_list(instr) RCURLY
    { Block(List.flatten lvar, li) }

instr:
  | b = instrBlock { b }
  | e = expr SEMICOLON { Expr(e) }
  | id = expr ASSIGN e = expr SEMICOLON { Assign(id, e) }
  | IF cond = expr THEN then_ = instr ELSE else_ = instr { Ite(cond, then_, else_) }
  | RETURN e = expr SEMICOLON { Return(e) }

expr:
  | c = CSTE { Cste(c) }
  | id = ID  { Id(id) }
  | s = STRLIT { String(s) }

  // those have conflicts
  | lhs = expr op = RELOP rhs = expr { BinOp(lhs, op, rhs) }
  | lhs = expr PLUS rhs = expr { BinOp(lhs, Add, rhs) }
  | lhs = expr MINUS rhs = expr { BinOp(lhs, Sub, rhs) }
  | lhs = expr DIV rhs = expr { BinOp(lhs, Div, rhs) }
  | lhs = expr TIMES rhs = expr { BinOp(lhs, Mul, rhs) }
  | lhs = expr STRCAT rhs = expr { StrCat(lhs, rhs) }
  | e = expr DOT f = ID LPAREN le = separated_list(COMMA, expr) RPAREN { Call(e, f, le) }
  | c = CLASSNAME DOT f = ID LPAREN le = separated_list(COMMA, expr) RPAREN { StaticCall(c, f, le) }
  | e = expr DOT name = ID { Attr(e, name) }
  | c = CLASSNAME DOT name = ID { StaticAttr(c, name) }

  | LPAREN e = expr RPAREN { e }
  | MINUS rhs = expr { UMinus (rhs) }
  | PLUS rhs = expr { rhs }
  | NEW name = CLASSNAME LPAREN le = separated_list(COMMA, expr) RPAREN { New(name, le) }
