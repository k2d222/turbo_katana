(* open Ast

let getenv env id =
  List.assoc_opt id env

let addenv env id c =
  match (getenv env id) with
    | Some(_) -> failwith ("env already contains " ^ id)
    | None -> (id, c) :: env

let rec interprete env e =

  let eval_op env (op, e1, e2) =
    let c1 = interprete env e1 in
    let c2 = interprete env e2 in
    match op with
      | Eq -> c1 = c2
      | Neq -> c1 != c2
      | Lt -> c1 < c2
      | Le -> c1 <= c2
      | Gt -> c1 > c2
      | Ge -> c1 >= c2 in

  let eval_id env id = match getenv env id with
    | Some(c) -> c
    | None -> failwith ("variable not found : " ^ id) in

  let eval_ite env (op, e1, e2) yes no =
      let e_yes = interprete env yes in
      let e_no = interprete env no in
      if eval_op env (op, e1, e2) then e_yes else e_no in

  match e with
    | Cste(c) -> c
    | Id(id) -> eval_id env id
    | Plus(e1, e2) -> (interprete env e1) + (interprete env e2)
    | Minus(e1, e2) -> (interprete env e1) - (interprete env e2)
    | Times(e1, e2) -> (interprete env e1) * (interprete env e2)
    | Div(e1, e2) -> (interprete env e1) / (interprete env e2)
    | UMinus(e) -> -(interprete env e)
    | Ite(Comp(op, e1, e2), yes, no) -> eval_ite env (op, e1, e2) yes no

let parse_decls ld =
  let rec rec_parse ld env =
    match ld with
      | [] -> env
      | decl::r ->
          let c = interprete env decl.rhs in
          let newenv = addenv env decl.lhs c in
          rec_parse r newenv
  in rec_parse ld []

let run_prog ast =
  let env = parse_decls ast.decls in
  let _ = Printf.printf "env : %s\n" (Misc.string_of_env env) in
  interprete env ast.expr *)
