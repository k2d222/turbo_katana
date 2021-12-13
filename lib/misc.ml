open Ast


let string_of_relop (op: Ast.opComp) =
  match op with
    Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let string_of_env env =
  let to_str (id, c) =
    Printf.sprintf "%s -> %i" id c in
  String.concat ", " (List.map to_str env)


let rec string_of_expr e =
  let string_of_cmp (op, e1, e2) =
    Printf.sprintf "%s %s %s" (string_of_expr e1) (string_of_relop op) (string_of_expr e2) in

  match e with
    | Cste(c) -> string_of_int c
    | Id(id) -> id
    | Plus(e1, e2) -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
    | Minus(e1, e2) -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
    | Times(e1, e2) -> (string_of_expr e1) ^ " × " ^ (string_of_expr e2)
    | Div(e1, e2) -> (string_of_expr e1) ^ " ÷ " ^ (string_of_expr e2)
    | UMinus(e) -> "-" ^ (string_of_expr e)
    | Ite(Comp(op, e1, e2), yes, no) ->
        Printf.sprintf "if %s then %s else %s"
        (string_of_cmp (op, e1, e2)) (string_of_expr yes) (string_of_expr no)
