%{
open Ast

type ctorParam = { param: Ast.param; isMember: bool }
type ctorDecl = { meth: Ast.methodDecl; super: superCall option; cparams: ctorParam list }

type classBodyElt =
  | StaticMethod of Ast.methodDecl
  | InstMethod of Ast.methodDecl
  | Ctor of ctorDecl
  | StaticAttrib of Ast.param list
  | InstAttrib of Ast.param list

let split_body_elts l =

  let rec r_partition l (lsm, lim, lc, lsa, lia) =
    match l with
    | [] -> (lsm, lim, lc, lsa, lia)
    | e::r ->
      let res =  (match e with
        | StaticMethod m -> (m::lsm, lim, lc, lsa, lia)
        | InstMethod m -> (lsm, m::lim, lc, lsa, lia)
        | Ctor c -> (lsm, lim, c::lc, lsa, lia)
        | StaticAttrib s -> (lsm, lim, lc, s@lsa, lia)
        | InstAttrib i -> (lsm, lim, lc, lsa, i@lia))
      in r_partition r res

  in r_partition l ([], [], [], [], [])
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
  CLASS name = CLASSNAME ctorParams = ctorParamList superclass = option(preceded(EXTENDS, CLASSNAME)) IS LCURLY l = list(classBodyElement) RCURLY {
    let (lsm, lim, lc, lsa, lia) = split_body_elts l

    in let ctor = 
      if List.length lc <> 1
      then raise (Syntax_error (Printf.sprintf "class '%s' defines %d constructor(s), expected 1" name (List.length lc)))
      else List.hd lc

    in let lia = lia @ (ctor.cparams |> List.filter_map (fun p -> if p.isMember then Some(p.param) else None))

    in if ctorParams <> ctor.cparams
      then raise (Syntax_error (Printf.sprintf "different parameters between class '%s' header and constructor definition" name));
    
    (match ctor.super, superclass with
    | None, None -> ()
    | Some{ name=s1; args=_ }, Some(s2) when s1 = s2 -> ()
    | _ -> raise (Syntax_error (Printf.sprintf "different super class between class '%s' header and constructor definition" name)));
      
    { name; super=ctor.super; staticMethods=lsm; instMethods=lim; ctor=ctor.meth; staticAttrs=lsa; instAttrs=lia }
  }

classBodyElement:
  | md = methodDecl { md }
  | cd = ctorDecl { cd }
  | attr = attrDecl { attr }

methodDecl:
  | DEF override = boption(OVERRIDE) name = ID params = paramList retType = option(preceded(COLON, CLASSNAME)) IS body = instrBlock {
    InstMethod({ name; override; params; retType; body;  })
  }
  | DEF override = boption(OVERRIDE) name = ID params = paramList COLON retType = CLASSNAME ASSIGN e = expr {
    InstMethod({ name; override; params; retType=Some(retType); body=Assign(Id("result"), e); })
  }
  | DEF STATIC name = ID params = paramList retType = option(preceded(COLON, CLASSNAME)) IS body = instrBlock {
    StaticMethod({ name; override=false; params; retType; body; })
  }
  | DEF STATIC name = ID params = paramList COLON retType = CLASSNAME ASSIGN e = expr {
    StaticMethod({ name; override=false; params; retType=Some(retType); body=Assign(Id("result"), e); })
  }

ctorDecl:
  | DEF name = CLASSNAME cparams = ctorParamList IS b = instrBlock {
    let instAttrs = cparams |> List.filter_map (fun p -> if p.isMember then Some(p.param) else None)
    in let params = cparams |> List.map (fun p -> p.param)
    in let prelude = instAttrs |> List.map (fun (p: param) -> Assign(Attr(Id("this"), p.name), Id(p.name))) 
    in let body = match b with
    | Block(lp, li) -> Block(lp, prelude @ li)
    | _ -> failwith "unreachable"
    in Ctor({ super=None; meth={ name; params; body; override=false; retType=None }; cparams })
  }
  | DEF name = CLASSNAME cparams = ctorParamList COLON super = CLASSNAME lsuper = superList IS b = instrBlock {
    let instAttrs = cparams |> List.filter_map (fun p -> if p.isMember then Some(p.param) else None)
    in let params = cparams |> List.map (fun p -> p.param)
    in let prelude = instAttrs |> List.map (fun (p: param) -> Assign(Attr(Id("this"), p.name), Id(p.name))) 
    in let body = match b with
    | Block(lp, li) -> Block(lp, prelude @ li)
    | _ -> failwith "unreachable"
    in Ctor({ super=Some{name=super; args=lsuper}; meth={ name; params; body; override=false; retType=None }; cparams })
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
  isMember = boption(VAR) params = param
  { List.map (fun param -> { isMember; param }) params }

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
  | RETURN SEMICOLON { Return }

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
  | LPAREN c = CLASSNAME e = expr RPAREN { StaticCast(c, e) }
