%{
open Ast
open Print

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
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token STRCAT
%token LPAREN RPAREN
%token LCURLY RCURLY
%token SEMICOLON COLON COMMA DOT
%token ASSIGN
%token EOF

// %right ELSE
// %left Comp
// %left PLUS MINUS
// %left TIMES DIV
// %left DOT

%type <Ast.prog> prog

%start prog
%%

prog:
  ld = list(classDecl) i = instr EOF { { decls=ld; instr=i } }

classDecl:
  CLASS cname = CLASSNAME params = paramList ext = extends IS body = classBody {
    { name=cname; ctorParams=params; body=body; superclass=ext }
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
  | DEF static = boption(STATIC) override = boption(OVERRIDE) name = ID params = paramList retType = option(preceded(COLON, CLASSNAME)) IS b = instrBlock {
     Method({ name=name; params=params;  retType=retType; body=b;  })
  }
  | DEF static = boption(STATIC) override = boption(OVERRIDE) name = ID params = paramList COLON retType = CLASSNAME ASSIGN e = expr {
    Method({ name=name; params=params; retType=Some(retType); body=Expr(e); })
  }

ctorDecl:
  | DEF name = CLASSNAME params = paramList IS b = instrBlock {
     Ctor({ name=name; params=params; superCall=None; body=b;  })
  }
  | DEF name = CLASSNAME params = paramList COLON super = CLASSNAME lsuper = superList IS b = instrBlock {
    Ctor({ name=name; params=params; superCall=Some(super, lsuper); body=b; })
  }


attrDecl:
  | VAR static = boption(STATIC) lname = separated_list(COMMA, ID) COLON clName = CLASSNAME SEMICOLON {
    let p = List.map (fun n -> { name=n; className=clName }) lname in
    if static then StaticAttrib(p) else InstAttrib(p)
  }

paramList:
  LPAREN lp = separated_list(COMMA, ctorParam) RPAREN { List.flatten lp }

superList:
  LPAREN le = separated_list(COMMA, expr) RPAREN { le }

ctorParam:
  varopt = boption(VAR) names = separated_nonempty_list(COMMA, ID) COLON clname = CLASSNAME
  { List.map (fun name -> { name=name; className=clname }) names }

extends:
  | EXTENDS id = CLASSNAME { Some(id) }
  | { None }

param:
  names = separated_nonempty_list(COMMA, ID) COLON clname = CLASSNAME { List.map (fun name -> { name=name; className=clname }) names }

instrBlock:
  | LCURLY li = list(instr) RCURLY { Block([], li) }
  | LCURLY lvar = separated_nonempty_list(COMMA, param) IS li = nonempty_list(instr) RCURLY
    { Block(List.flatten lvar, li) }

instr:
  | b = instrBlock { b }
  | e = expr SEMICOLON { Expr(e) }
  | id = expr ASSIGN e = expr SEMICOLON { Assign(id, e) }
  | IF cond = expr THEN then_ = instr ELSE else_ = instr { Ite(cond, then_, else_) }
  | RETURN e = expr SEMICOLON { Return(e) }

expr:
  | c = CSTE { Cste(c) }
  | id = ID | id = CLASSNAME { Id(id) }
  | LPAREN e = expr RPAREN { e }
  | s = STRLIT { String(s) }
  | e = expr DOT name = ID { Select(e, name) }
  | lhs = expr PLUS rhs = expr { Plus(lhs, rhs) }
  | lhs = expr MINUS rhs = expr { Minus(lhs, rhs) }
  | lhs = expr DIV rhs = expr { Div(lhs, rhs) }
  | lhs = expr TIMES rhs = expr { Times(lhs, rhs) }
  | lhs = expr STRCAT rhs = expr { StrCat(lhs, rhs) }
  | MINUS rhs = expr { UMinus (rhs) }
  | PLUS rhs = expr { rhs }
  | lhs = expr DOT rhs = expr { AttrOf(lhs, rhs) }
  | e = expr LPAREN le = separated_list(COMMA, expr) RPAREN { MethodCall(e, le) } (**TODO *)
  | lhs = expr op = RELOP rhs = expr { Comp(lhs, op, rhs) }
  | NEW name = CLASSNAME LPAREN le = separated_list(COMMA, expr) RPAREN { New(name, le) }
