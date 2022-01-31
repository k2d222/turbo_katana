open Ast

let string_of_expr_type e =
  match e with
  | Id _ -> "identifier"
  | Cste _ -> "constant"
  | Attr _ -> "instance attribute"
  | StaticAttr _ -> "static attribute"
  | UMinus _ -> "unary minus"
  | Call _ -> "method call"
  | StaticCall _ -> "static method call"
  | BinOp _ -> "numeric operation"
  | String _ -> "string literal"
  | StrCat _ -> "string concatenation"
  | New _ -> "instantiation"
  | StaticCast _ -> "static cast"
