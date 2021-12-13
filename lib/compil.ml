(* open Ast

let _NOP = Printf.sprintf "NOP\n"
let _ERR = Printf.sprintf "ERR %s\n"
let _START = Printf.sprintf "START\n"
let _STOP = Printf.sprintf "STOP\n"
let _PUSHI = Printf.sprintf "PUSHI %i\n"
let _PUSHS = Printf.sprintf "PUSHS %s\n"
let _PUSHG = Printf.sprintf "PUSHG %i\n"
let _PUSHL = Printf.sprintf "PUSHL %i\n"
let _PUSHSP = Printf.sprintf "PUSHSP %i\n"
let _PUSHFP = Printf.sprintf "PUSHFP %i\n"
let _STOREL = Printf.sprintf "STOREL %i\n"
let _STOREG = Printf.sprintf "STOREG %i\n"
let _PUSHN = Printf.sprintf "PUSHN %i\n"
let _POPN = Printf.sprintf "POPN %i\n"
let _DUPN = Printf.sprintf "DUPN %i\n"
let _SWAP = Printf.sprintf "SWAP\n"
let _EQUAL = Printf.sprintf "EQUAL\n"
let _NOT = Printf.sprintf "NOT\n"
let _JUMP = Printf.sprintf "JUMP %s\n"
let _JZ = Printf.sprintf "JZ %s\n"
let _PUSHA = Printf.sprintf "PUSHA %s\n"
let _CALL = Printf.sprintf "CALL\n"
let _RETURN = Printf.sprintf "RETURN\n"
let _ADD = Printf.sprintf "ADD\n"
let _SUB = Printf.sprintf "SUB\n"
let _MUL = Printf.sprintf "MUL\n"
let _DIV = Printf.sprintf "DIV\n"
let _INF = Printf.sprintf "INF\n"
let _INFEQ = Printf.sprintf "INFEQ\n"
let _SUP = Printf.sprintf "SUP\n"
let _SUPEQ = Printf.sprintf "SUPEQ\n"
let _WRITEI = Printf.sprintf "WRITEI\n"
let _STR = Printf.sprintf "STR\n"
let _WRITES = Printf.sprintf "WRITES\n"
let _CONCAT = Printf.sprintf "CONCAT\n"
let _STORE = Printf.sprintf "STORE %d\n"
let _LOAD = Printf.sprintf "LOAD %d\n"
let _ALLOC = Printf.sprintf "ALLOC %d\n"
let _LABEL = Printf.sprintf "%s: NOP\n"
let _COMMENT = Printf.sprintf "-- %s\n"

let lbl_counter = ref 0

let genlbl () =
  lbl_counter := !lbl_counter + 1;
  "lbl" ^ string_of_int !lbl_counter

let addr_of env id = match List.assoc_opt id env with
  | Some(addr) -> addr
  | None -> failwith (Printf.sprintf "variable %s not found in env" id)

let compile_expr env e =
  let op_to_code op = match op with
    | Eq -> _EQUAL
    | Neq -> _EQUAL ^ _NOT
    | Lt -> _INF
    | Le -> _INFEQ
    | Gt -> _SUP
    | Ge -> _SUPEQ in

  let rec code e =
    let code_cmp (op, e1, e2) =
      code e1 ^ code e2 ^ (op_to_code op) in

    let code_ite cmp yes no =
      let lbl_else = genlbl () in
      let lbl_end = genlbl () in
      (code_cmp cmp)
      ^ _JZ lbl_else
      ^ (code yes)
      ^ _JUMP lbl_end
      ^ _LABEL lbl_else
      ^ (code no)
      ^ _LABEL lbl_end in

    (match e with
      | Cste(c) -> _PUSHI c
      | Id(id) -> _PUSHG (addr_of env id)
      | Plus(e1, e2) -> code e1 ^ code e2 ^ _ADD
      | Minus(e1, e2) -> code e1 ^ code e2 ^ _SUB
      | Times(e1, e2) -> code e1 ^ code e2 ^ _MUL
      | Div(e1, e2) -> code e1 ^ code e2 ^ _DIV
      | UMinus(e) -> _PUSHI 0 ^ code e ^ _SUB
      | Ite(Comp(op, e1, e2), yes, no) -> code_ite (op, e1, e2) yes no) in

  code e

let addenv env id = match env with
  | [] -> [(id, 0)]
  | (_, addr)::r -> (id, addr + 1)::env

let compile_decl env decl =
  let newenv = addenv env decl.lhs in
  let addr = snd (List.hd newenv) in
  (newenv, compile_expr env decl.rhs ^ _DUPN 1 ^ _STOREG addr)

let compile_decls decls =
  List.fold_left (fun (env, code) decl ->
    let (newenv, newcode) = compile_decl env decl in
    (newenv, code ^ newcode)) ([], "") decls

let compile ast =
  let (env, declcode) = compile_decls ast.decls in
  let exprcode = compile_expr env ast.expr in
  _START
  ^ _COMMENT "declarations :"
  ^ declcode
  ^ _COMMENT "expression :"
  ^ exprcode
  ^ _WRITEI
  ^ _STOP *)
